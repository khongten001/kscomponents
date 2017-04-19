unit ksLoadingIndicator;

interface

uses FMX.Forms, Classes, FMX.Controls, FMX.Objects, ksTypes, FMX.Graphics,
  FMX.StdCtrls;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64
    {$ELSE} pidiOSDevice {$ENDIF} or pidiOSSimulator or pidAndroid)]
  TksLoadingIndicator = class(TRectangle)
  private
    FLoadingText: string;
    FFadeBackground: Boolean;
    FIsModal: Boolean;
    FLabel: TLabel;
    procedure SetIsModal(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowLoading;
    procedure HideLoading;
  published
    property IsModal: Boolean read FIsModal write SetIsModal default False;
    property LoadingText: string read FLoadingText write FLoadingText;
    property FadeBackground: Boolean read FFadeBackground write FFadeBackground default False;
  end;


  procedure ShowLoadingIndicator(AForm: TCommonCustomForm);
  procedure HideLoadingIndicator(AForm: TCommonCustomForm);



procedure Register;

implementation

uses System.UIConsts, FMX.Types, SysUtils, Types, FMX.Ani;

var
  APos: TPointF;


procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksLoadingIndicator]);
end;



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

procedure ShowLoadingIndicator(AForm: TCommonCustomForm);
var
  ALoadingIndicator: TksLoadingIndicator;
begin
  ALoadingIndicator := FindLoadingIndicator(AForm);
  if ALoadingIndicator = nil then
    ALoadingIndicator := TksLoadingIndicator.Create(AForm);
  //ALoadingIndicator.Opacity := 0;

  if APos.X > 0 then
  begin
    ALoadingIndicator.Position.X := APos.X;
    ALoadingIndicator.Position.Y := APos.Y;
  end
  else
  begin
    ALoadingIndicator.Position.X := (AForm.Width-100) / 2;
    ALoadingIndicator.Position.Y := (AForm.Height-100) / 2;
    APos.X := ALoadingIndicator.Position.X;
    APos.Y := ALoadingIndicator.Position.Y;
  end;

  AForm.AddObject(ALoadingIndicator);

  ALoadingIndicator.BringToFront;
  Application.ProcessMessages;
  //Sleep(100);
  //TAnimator.AnimateFloatWait(ALoadingIndicator, 'Opacity', 1);
end;

procedure HideLoadingIndicator(AForm: TCommonCustomForm);
var
  ALoadingIndicator: TksLoadingIndicator;
begin
  ALoadingIndicator := FindLoadingIndicator(AForm);
  //TAnimator.AnimateFloat(ALoadingIndicator, 'Opacity', 0);
  if ALoadingIndicator <> nil then
    AForm.RemoveObject(ALoadingIndicator);
end;

{ TksLoadingIndicatorExt }
               {
constructor TksLoadingIndicatorExt.Create(AOwner: TComponent);
begin
  inherited;
  Fill.Color := claBlack;
  Width := 100;
  Height := 100;
  XRadius := 10;
  YRadius := 10;
  Opacity := 0.75;
  Stroke.Kind := TBrushKind.None;
  Visible := False;
end;

procedure TksLoadingIndicatorExt.Paint;
begin
  inherited;
  Canvas.Fill.Color := claWhite;
  Canvas.Font.Size := 16;
  Canvas.FillText(ClipRect, 'LOADING', False, 1, [], TTextAlign.Center, TTextAlign.Center);
end;
                 }
{ TksLoadingIndicator }

constructor TksLoadingIndicator.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := False;
  FLoadingText := 'LOADING';
  FFadeBackground := False;
  FIsModal := False;
  Fill.Color := claBlack;
  Width := 100;
  Height := 100;
  {$IFNDEF ANDROID}
  XRadius := 10;
  YRadius := 10;
  {$ELSE}
  Opacity := 0.7;
  {$ENDIF}

  //Align := TAlignLayout.Center;

  FLabel := TLabel.Create(Self);
  FLabel.Align := TAlignLayout.Client;
  FLabel.TextSettings.FontColor := claWhite;
  FLabel.Text := 'LOADING';
  FLabel.TextSettings.HorzAlign := TTextAlign.Center;
  FLabel.TextSettings.VertAlign := TTextAlign.Center;
  FLabel.StyledSettings := [];
  AddObject(FLabel);
end;


destructor TksLoadingIndicator.Destroy;
begin
  inherited;
end;

procedure TksLoadingIndicator.HideLoading;
begin
  HideLoadingIndicator(Owner as TForm);
end;


procedure TksLoadingIndicator.SetIsModal(const Value: Boolean);
begin
  FIsModal := Value;
end;

procedure TksLoadingIndicator.ShowLoading;
begin
  ShowLoadingIndicator(Owner as TForm);
end;

initialization

  APos := PointF(0, 0);
  //ALoadingIndicator := TksLoadingIndicator.Create(nil);

finalization
 //
 // ALoadingIndicator.DisposeOf;

end.
