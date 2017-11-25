unit ksLoadingIndicator;

interface

uses FMX.Forms, Classes, FMX.Controls, FMX.Objects, ksTypes, FMX.Graphics,
  FMX.StdCtrls, FMX.Layouts
  {$IFDEF IOS}
  , iOSapi.UIKit, iOSapi.Foundation
  {$ENDIF}
  ;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64
    {$ELSE} pidiOSDevice {$ENDIF} or pidiOSSimulator or pidAndroid)]



  TksLoadingIndicator = class(TLayout)
  private
    FRectangle: TRectangle;
    FBackground: TRectangle;
    FLoadingText: string;
    FFadeBackground: Boolean;
    FIsModal: Boolean;
    FLabel: TLabel;
    procedure SetIsModal(const Value: Boolean);
    procedure SetFadeBackground(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowLoading;
    procedure HideLoading;
  published
    property IsModal: Boolean read FIsModal write SetIsModal default False;
    property LoadingText: string read FLoadingText write FLoadingText;
    property FadeBackground: Boolean read FFadeBackground write SetFadeBackground default False;
  end;


  procedure ShowLoadingIndicator(AForm: TCommonCustomForm;
                                 const AFade: Boolean = False;
                                 const AModal: Boolean = False);
  procedure HideLoadingIndicator(AForm: TCommonCustomForm);
  function IsLoadingIndicatorVisible(AForm: TCommonCustomForm): Boolean;
  function FindLoadingIndicator(AForm: TCommonCustomForm): TksLoadingIndicator;



implementation

uses
  System.UIConsts, FMX.Types, SysUtils, Types, FMX.Ani
  {$IFDEF IOS}
  ,iOSapi.CoreGraphics, FMX.Helpers.iOS
  {$ENDIF}
  ;



function FindLoadingIndicator(AForm: TCommonCustomForm): TksLoadingIndicator;
var
  ICount: integer;
begin
  Result := nil;
  if AForm = nil then
    Exit;
  for ICount := AForm.ComponentCount-1 downto 0 do
  begin
    if AForm.Components[ICount] is TksLoadingIndicator then
    begin
      Result := (AForm.Components[ICount] as TksLoadingIndicator);
      Exit;
    end;
  end;
end;

function IsLoadingIndicatorVisible(AForm: TCommonCustomForm): Boolean;
var
  ALoading: TksLoadingIndicator;
begin
  Result := False;
  ALoading := FindLoadingIndicator(AForm);
  if ALoading <> nil then
  begin
    if ALoading.Parent = AForm then
      Result := True;
  end;
end;

procedure ShowLoadingIndicator(AForm: TCommonCustomForm;
                               const AFade: Boolean = False;
                               const AModal: Boolean = False);
var
  ALoadingIndicator: TksLoadingIndicator;
begin
  Application.ProcessMessages;
  ALoadingIndicator := FindLoadingIndicator(AForm);
  if ALoadingIndicator = nil then
    ALoadingIndicator := TksLoadingIndicator.Create(AForm);

  ALoadingIndicator.FadeBackground := AFade;
  ALoadingIndicator.IsModal := AModal;

  AForm.AddObject(ALoadingIndicator);

  ALoadingIndicator.BringToFront;
  Application.ProcessMessages;
end;

procedure HideLoadingIndicator(AForm: TCommonCustomForm);
var
  ALoadingIndicator: TksLoadingIndicator;
begin
  if AForm = nil then
    Exit;

  ALoadingIndicator := FindLoadingIndicator(AForm);
  if ALoadingIndicator <> nil then
    AForm.RemoveObject(ALoadingIndicator);

end;

{ TksLoadingIndicator }

constructor TksLoadingIndicator.Create(AOwner: TComponent);
begin
  inherited;

  Align := TAlignLayout.Client;

  HitTest := False;
  FLoadingText := 'LOADING';
  FFadeBackground := False;
  FIsModal := False;

  FBackground := TRectangle.Create(Self);
  FBackground.Align := TAlignLayout.Client;
  FBackground.Stroke.Kind := TBrushKind.None;
  FBackground.Fill.Kind := TBrushKind.Solid;
  FBackground.Fill.Color := claBlack;
  FBackground.HitTest := False;
  FBackground.Opacity := 0.3;
  AddObject(FBackground);


  FRectangle := TRectangle.Create(Self);
  FRectangle.Align := TAlignLayout.Center;
  FRectangle.Stroke.Kind := TBrushKind.None;
  FRectangle.Stroke.Color := claNull;
  FRectangle.Fill.Color := claBlack;
  FRectangle.Width := 90;
  FRectangle.Height := 70;

  FRectangle.XRadius := 5;
  FRectangle.YRadius := 5;

  FRectangle.Opacity := 1;

  FLabel := TLabel.Create(FRectangle);
  FLabel.Align := TAlignLayout.Client;
  FLabel.TextSettings.FontColor := claWhite;
  FLabel.Text := FLoadingText;
  FLabel.TextSettings.HorzAlign := TTextAlign.Center;
  FLabel.TextSettings.VertAlign := TTextAlign.Center;
  FLabel.StyledSettings := [];

  FRectangle.AddObject(FLabel);
  AddObject(FRectangle);
end;


destructor TksLoadingIndicator.Destroy;
begin
  inherited;
end;

procedure TksLoadingIndicator.HideLoading;
begin
  HideLoadingIndicator(Owner as TForm);
end;


procedure TksLoadingIndicator.SetFadeBackground(const Value: Boolean);
begin
  FFadeBackground := Value;
  case Value of
    True: FBackground.Opacity := 0.3;
    False: FBackground.Opacity := 0;
  end;
end;

procedure TksLoadingIndicator.SetIsModal(const Value: Boolean);
begin
  FIsModal := Value;
  FBackground.HitTest := Value;
end;

procedure TksLoadingIndicator.ShowLoading;
begin
  ShowLoadingIndicator(Owner as TForm);

end;


end.



