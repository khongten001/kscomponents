{*******************************************************************************
*                                                                              *
*  TksPushNotification - Push Notification Component                           *
*                                                                              *
*  https://bitbucket.org/gmurt/kscomponents                                    *
*                                                                              *
*  Copyright 2017 Graham Murt                                                  *
*                                                                              *
*  email: graham@kernow-software.co.uk                                         *
*                                                                              *
*  Licensed under the Apache License, Version 2.0 (the "License");             *
*  you may not use this file except in compliance with the License.            *
*  You may obtain a copy of the License at                                     *
*                                                                              *
*    http://www.apache.org/licenses/LICENSE-2.0                                *
*                                                                              *
*  Unless required by applicable law or agreed to in writing, software         *
*  distributed under the License is distributed on an "AS IS" BASIS,           *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    *
*  See the License for the specific language governing permissions and         *
*  limitations under the License.                                              *
*                                                                              *
*******************************************************************************}

unit ksPushNotification;

interface

{$I ksComponents.inc}

uses Classes, FMX.MediaLibrary, FMX.Media, FMX.Platform, System.Messaging, FMX.Graphics,
  ksTypes, System.PushNotification;

type
  TksReceivePushTokenEvent = procedure(Sender: TObject; AToken: string) of object;
  TksReceivePushMessageEvent = procedure(Sender: TObject; ATitle, AMessage: string; ABadge: integer) of object;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64
    {$ELSE} pidiOSDevice {$ENDIF} or pidiOSSimulator or pidAndroid)]
  TksPushNotification = class(TksComponent)
  private
    FFirebaseSenderID: string;

    FServiceConnection: TPushServiceConnection;
    FPushService: TPushService;
    FDeviceToken: string;
    // events...
    FOnReceiveTokenEvent: TksReceivePushTokenEvent;
    FOnReceivePushMessageEvent: TksReceivePushMessageEvent;
    procedure OnNotificationChange(Sender: TObject; AChange: TPushService.TChanges);
    procedure OnReceiveNotificationEvent(Sender: TObject; const ANotification: TPushServiceNotification);
  protected
    procedure Loaded; override;
  public
    procedure Activate;
  published
    property FirebaseSenderID: string read FFirebaseSenderID write FFirebaseSenderID stored True;
    property OnReceiveToken: TksReceivePushTokenEvent read FOnReceiveTokenEvent write FOnReceiveTokenEvent;
    property OnReceivePushMessageEvent: TksReceivePushMessageEvent read FOnReceivePushMessageEvent write FOnReceivePushMessageEvent;
  end;

  procedure Register;

implementation

uses Types, SysUtils, System.Threading
  {$IFDEF XE10_OR_NEWER} ,FMX.DialogService {$ELSE} , FMX.Dialogs {$ENDIF}

  {$IFDEF IOS}
  , FMX.PushNotification.IOS
  {$ENDIF}

  {$IFDEF ANDROID}
  , FMX.PushNotification.Android,
  Androidapi.Helpers,
  FMX.Helpers.Android,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Net,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Telephony
  {$ENDIF}
;



procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksPushNotification]);
end;

{ TksCamara }

procedure TksPushNotification.Activate;
{$IFDEF ANDROID}
var
  ATask: ITask;
{$ENDIF}
begin
  {$IFDEF IOS}
  FServiceConnection.Active := True;
  {$ENDIF}
  {$IFDEF ANDROID}
  ATask := TTask.Create (procedure ()
     begin
        try
          FServiceConnection.Active := True;
        except
          on E:Exception do
          begin
            TThread.Synchronize(nil,
            procedure
            begin
              TDialogService.ShowMessage(e.Message);
            end);
          end;
        end;
     end);
   ATask.Start;
  {$ENDIF}
end;



procedure TksPushNotification.Loaded;
var
  AServiceName: string;
begin
  inherited;
  {$IFDEF IOS}
  AServiceName := TPushService.TServiceNames.APS;
  {$ENDIF}
  {$IFDEF ANDROID}
  AServiceName := TPushService.TServiceNames.GCM;
  {$ENDIF}
  FPushService := TPushServiceManager.Instance.GetServiceByName(AServiceName);
  {$IFDEF ANDROID}
  FPushService.AppProps[TPushService.TAppPropNames.GCMAppID] := FFirebaseSenderID;
  {$ENDIF}
  FServiceConnection := TPushServiceConnection.Create(FPushService);
  FServiceConnection.OnChange := OnNotificationChange;
  FServiceConnection.OnReceiveNotification := OnReceiveNotificationEvent;
end;

procedure ShowMsg(AMsg: string);
begin
  {$IFDEF XE10_OR_NEWER}
  TDialogService.ShowMessage(AMsg);
  {$ELSE}
  ShowMessage(AMsg);
  {$ENDIF}
end;

procedure TksPushNotification.OnNotificationChange(Sender: TObject; AChange: TPushService.TChanges);

var
  AToken: string;
begin
  if (TPushService.TChange.Status in AChange) then
  begin
    if (FPushService.Status = TPushService.TStatus.StartupError) then
    begin
      TThread.Synchronize(nil,
        procedure

        begin
          if Pos('java.lang.securityexception', LowerCase(FPushService.StartupError)) > 0 then

            ShowMsg('Unable to activate push notifications...'+#13+#13+
                    'Check that you have enabled "Receive Push Notifications" in Options->Entitlement List')
          else
          if Pos('invalid_sender', LowerCase(FPushService.StartupError)) > 0 then
            ShowMsg('Unable to activate push notifications...'+#13+#13+
                                       'The FirebaseSenderID value is invalid.')
          else
            ShowMsg(FPushService.StartupError);
        end);
      Exit;
    end;
  end;

  AToken := Trim(FPushService.DeviceTokenValue[TPushService.TDeviceTokenNames.DeviceToken]);
  if AToken <> '' then
  begin
    FDeviceToken := AToken;
    TThread.Synchronize(nil,
      procedure
      begin
        if Assigned(FOnReceiveTokenEvent) then
          FOnReceiveTokenEvent(Self, FDeviceToken);
      end);
  end;
end;


procedure TksPushNotification.OnReceiveNotificationEvent(Sender: TObject;
  const ANotification: TPushServiceNotification);
begin
  if Assigned(FOnReceivePushMessageEvent) then
  begin
    {$IFDEF IOS}
    FOnReceivePushMessageEvent(Self,
                               '',
                               ANotification.DataObject.Values['alert'].Value,
                               StrToIntDef(ANotification.DataObject.Values['badge'].Value, 0));
    {$ENDIF}
    {$IFDEF ANDROID}
    FOnReceivePushMessageEvent(Self,
                               ANotification.DataObject.Values['title'].Value,
                               ANotification.DataObject.Values['message'].Value,
                               0);
    {$ENDIF}
  end;
end;

end.
