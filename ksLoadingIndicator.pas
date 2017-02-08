unit ksLoadingIndicator;

interface

uses FMX.Forms, Classes, FMX.Controls, FMX.Objects, ksTypes;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64
    {$ELSE} pidiOSDevice {$ENDIF} or pidiOSSimulator or pidAndroid)]
  TksLoadingIndicator = class(TksComponent)
  private
    FLoadingText: string;
    FFadeBackground: Boolean;
    FIsModal: Boolean;
    procedure SetIsModal(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ShowLoading;
    procedure HideLoading;
  published
    property IsModal: Boolean read FIsModal write SetIsModal default False;
    property LoadingText: string read FLoadingText write FLoadingText;
    property FadeBackground: Boolean read FFadeBackground write FFadeBackground default False;
  end;


  procedure ShowLoadingIndicator(AForm: TForm);
  procedure HideLoadingIndicator(AForm: TForm);

  procedure Register;

implementation

uses System.UIConsts, FMX.StdCtrls, FMX.Types, FMX.Graphics;

type
  TksLoadingIndicatorExt = class(TRectangle)
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;

  end;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksLoadingIndicator]);
end;

function FindLoadingIndicator(AForm: TForm): TksLoadingIndicatorExt;
var
  ICount: integer;
begin
  Result := nil;
  for ICount := AForm.ComponentCount-1 downto 0 do
  begin
    if AForm.Components[ICount] is TksLoadingIndicatorExt then
    begin
      Result := (AForm.Components[ICount] as TksLoadingIndicatorExt);
      Exit;
    end;
  end;
end;

procedure ShowLoadingIndicator(AForm: TForm);
var
  ALoading: TksLoadingIndicatorExt;
begin
  ALoading := FindLoadingIndicator(AForm);
  if ALoading <> nil then
  begin
    ALoading.Position.X := (AForm.ClientWidth/2) - (ALoading.Width / 2);
    ALoading.Position.Y := (AForm.ClientHeight/2) - (ALoading.Width / 2);
    ALoading.Visible := True;
    ALoading.BringToFront;
    Application.ProcessMessages;
    Exit;
  end;
  ALoading := TksLoadingIndicatorExt.Create(AForm);
  AForm.AddObject(ALoading);
  ShowLoadingIndicator(AForm);

  //ALoading.Parent := AForm;
  //ALoading.Visible := True;

end;

procedure HideLoadingIndicator(AForm: TForm);
var
  ICount: integer;
begin
  for ICount := AForm.ComponentCount-1 downto 0 do
  begin
    if AForm.Components[ICount] is TksLoadingIndicatorExt then
    begin
      (AForm.Components[ICount] as TksLoadingIndicatorExt).Visible := False;;
    end;
  end;
  {$IFDEF ANDROID}
  Application.ProcessMessages;
  {$ENDIF}
end;

{ TksLoadingIndicatorExt }

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

{ TksLoadingIndicator }

constructor TksLoadingIndicator.Create(AOwner: TComponent);
begin
  inherited;
  FLoadingText := 'LOADING';
  FFadeBackground := False;
  FIsModal := False;
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

end.
