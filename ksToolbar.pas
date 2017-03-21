{*******************************************************************************
*                                                                              *
*  TkToolbar - Toolbar with form stack/transitions awareness                   *
*                                                                              *
*  https://github.com/gmurt/KernowSoftwareFMX                                  *
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

unit ksToolBar;

interface

{$I ksComponents.inc}


uses Classes, FMX.StdCtrls, FMX.Graphics, ksTypes, FMX.Objects, FMX.Types,
  System.UITypes, System.UIConsts, ksSpeedButton, ksFormTransition;

type
  IksToolbar = interface
  ['{42609FB8-4DE0-472F-B49C-A6CD636A530D}']
    procedure SetTransition(ATransition: TksTransitionType);
  end;

  TksToolbarButton = class(TPersistent)
  private
    FButton: TksSpeedButton;
    FText: string;
    //FStyleLookup: string;
    procedure SetText(const Value: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property Text: string read FText write SetText;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64
    {$ELSE} pidiOSDevice {$ENDIF} or pidiOSSimulator or pidAndroid)]
  TksToolbar = class(TToolBar, IksToolbar)
  private
    FLabel: TLabel;


    FBackButton: TksToolbarButton;
    FRightButton: TksToolbarButton;

    FText: string;
    FFormTransition: TksFormTransition;
    FOnRightButtonClicked: TNotifyEvent;
    procedure BackButtonClicked(Sender: TObject);
    procedure RightButtonClicked(Sender: TObject);
    procedure SetText(const Value: string);
  protected
    procedure SetTransition(ATransition: TksTransitionType);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Text: string read FText write SetText;
    property RightButton: TksToolbarButton read FRightButton write FRightButton;
    property OnRightButtonClicked: TNotifyEvent read FOnRightButtonClicked write FOnRightButtonClicked;
  end;


  procedure Register;

implementation

uses Math, System.TypInfo, System.Types, ksCommon, SysUtils,
  Fmx.Forms, FMX.Controls;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksToolbar]);
end;


{ TksToolbar }

procedure TksToolbar.BackButtonClicked(Sender: TObject);
begin
  FFormTransition.Pop;
end;

constructor TksToolbar.Create(AOwner: TComponent);
begin
  inherited;
  FFormTransition := TksFormTransition.Create(nil);

  FLabel := TLabel.Create(Self);
  FLabel.Align := TAlignLayout.Contents;
  FLabel.TextSettings.HorzAlign := TTextAlign.Center;
  FLabel.Stored := False;
  AddObject(FLabel);
  Text := 'TITLE';

  FBackButton := TksToolbarButton.Create;
  FBackButton.FButton.Align := TAlignLayout.Left;
  FBackButton.FButton.Stored := False;
  FBackButton.FButton.Visible := False;
  FBackButton.FButton.StyleLookup := 'backtoolbutton';
  FBackButton.FButton.Text := 'BACK';
  FBackButton.FButton.OnClick := BackButtonClicked;

  //AddObject(FBackButton);

  FRightButton := TksToolbarButton.Create;
  FRightButton.FButton.Align := TAlignLayout.Right;
  FRightButton.FButton.Stored := False;
  FRightButton.FButton.StyleLookup := 'toolbutton';
  FRightButton.FButton.OnClick := RightButtonClicked;

  AddObject(FRightButton.FButton);

  {FRightButton := TksSpeedButton.Create(Self);
  FRightButton.Align := TAlignLayout.Right;
  FRightButton.Stored := False;
  FRightButton.Text := 'BUTTON';
  FRightButton.StyledSettings := [TStyledSetting.Family,TStyledSetting.Style,TStyledSetting.FontColor];
  FRightButton.TextSettings.Font.Size := 14;
  FRightButton.OnClick := RightButtonClicked;    }
  //AddObject(FRightButton);

end;

destructor TksToolbar.Destroy;
begin
  FreeAndNil(FFormTransition);

  inherited;
end;

procedure TksToolbar.RightButtonClicked(Sender: TObject);
begin
  if Assigned(FOnRightButtonClicked) then
    FOnRightButtonClicked(Self);
end;

procedure TksToolbar.SetText(const Value: string);
begin
  FText := Value;
  FLabel.Text := FText;
end;

procedure TksToolbar.SetTransition(ATransition: TksTransitionType);
begin
  AddObject(FBackButton.FButton);

  FBackButton.FButton.Visible := True;
  if ATransition = TksTransitionType.ksFtSlideInFromRight then
  begin
    FBackButton.FButton.StyleLookup := 'backtoolbutton';
    FBackButton.FButton.Text := 'BACK';
  end
  else
  begin
    FBackButton.FButton.StyleLookup := 'toolbutton';
    FBackButton.FButton.Text := 'CLOSE';
  end;
end;

{ TksToolbarButton }

constructor TksToolbarButton.Create;
begin
  FButton := TksSpeedButton.Create(nil);
  FButton.Stored := False;
  FButton.StyledSettings := [TStyledSetting.Family,TStyledSetting.Style,TStyledSetting.FontColor];
  FButton.TextSettings.Font.Size := 14;
  FButton.Locked := True;
  FButton.Visible := False;
end;

destructor TksToolbarButton.Destroy;
begin
  FButton.DisposeOf;
  inherited;
end;

procedure TksToolbarButton.SetText(const Value: string);
begin
  FText := Value;
  FButton.Text := Value;
  FButton.Visible := FText <> '';
end;

initialization

  Classes.RegisterClass(TksToolbar);

end.
