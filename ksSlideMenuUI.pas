unit ksSlideMenuUI;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  ksTypes, ksVirtualListView, FMX.Effects, ksAppEvents;

type
  TfrmSlideMenuUI = class(TForm)
    ksVirtualListView1: TksVirtualListView;
    PaintBox1: TPaintBox;
    procedure _Image1Click(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure PaintBox1Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure ksVirtualListView1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  private
    FCanSelect: Boolean;
    FOnSelectItem: TksVListItemClickEvent;
    FBitmap: TBitmap;
    FAppEvents: TksAppEvents;
    FCallingForm: TCommonCustomForm;
    procedure Delay;
    procedure WillBecomeActive(Sender: TObject);
    { Private declarations }
  protected
    procedure DoShow; override;
    procedure SelectItem(Sender: TObject; AItem: TksVListItem);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenMenu(ACallingForm: TCommonCustomForm);
    procedure CloseMenu;
    property OnSelectItem: TksVListItemClickEvent read FOnSelectItem write FOnSelectItem;
    property Bitmap: TBitmap read FBitmap;
    { Public declarations }
  end;


implementation

uses FMX.Ani, ksCommon, ksSlideMenu, DateUtils;

{$R *.fmx}

{ TfrmSlideMenuUI }

constructor TfrmSlideMenuUI.Create(AOwner: TComponent);
begin
  inherited;
  FAppEvents := TksAppEvents.Create(nil);
  FBitmap := TBitmap.Create;
  FAppEvents.WillBecomeForeground := WillBecomeActive;
  //FBitmap.
end;

procedure TfrmSlideMenuUI.Delay;
var
  ANow: TDatetime;
begin
  ANow := Now;
  while MilliSecondsBetween(ANow, Now) < 100 do
    Application.ProcessMessages;
end;

destructor TfrmSlideMenuUI.Destroy;
begin
  FreeAndNil(FBitmap);
  FAppEvents.DisposeOf;
  inherited;
end;

procedure TfrmSlideMenuUI.WillBecomeActive(Sender: TObject);
{$IFDEF ANDROID}
var
  ABmp: TBitmap;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  ABmp := TBitmap.Create;
  try
    GenerateFormImageExt(FCallingForm, ABmp);
    Bitmap.Assign(ABmp);
  finally
    FreeAndNil(ABmp);
  end;
  {$ENDIF}
end;

procedure TfrmSlideMenuUI.CloseMenu;
begin
  Delay;
  TAnimator.AnimateFloatWait(PaintBox1, 'Position.X', 0, 0.2, TAnimationType.InOut, TInterpolationType.Sinusoidal);
  Visible := False;
end;

procedure TfrmSlideMenuUI.DoShow;
begin
  inherited;
  Delay;
  TAnimator.AnimateFloatWait(PaintBox1, 'Position.X', C_DEFAULT_MENU_WIDTH, 0.2, TAnimationType.InOut, TInterpolationType.Sinusoidal);
  FCanSelect := True;
end;

procedure TfrmSlideMenuUI.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkHardwareBack then

    PaintBox1Click(Self); //Image1Click(Self);
  Key := 0;
end;

procedure TfrmSlideMenuUI.ksVirtualListView1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if ksVirtualListView1.Items.ItemAtPos(x, y) = nil then
  begin
    CloseMenu;
    FCallingForm.Visible := True;
  end;
end;

procedure TfrmSlideMenuUI._Image1Click(Sender: TObject);
begin
  SelectItem(Self, ksVirtualListView1.Items[ksVirtualListView1.ItemIndex]);
end;

procedure TfrmSlideMenuUI.OpenMenu(ACallingForm: TCommonCustomForm);
var
  ABmp: TBitmap;
begin
  FCallingForm := ACallingForm;
  FCanSelect := False;
  if ksVirtualListView1.ItemIndex = -1 then
    ksVirtualListView1.ItemIndex := 0;
  ksVirtualListView1.OnItemClick := SelectItem;
  ksVirtualListView1.Width := C_DEFAULT_MENU_WIDTH;
  {Image1.Bitmap := GenerateFormImageExt(ACallingForm);
  Image1.SetBounds(0, 0, ACallingForm.Width, ACallingForm.Height);
   }

   //PaintBox1.Bitmap := GenerateFormImageExt(ACallingForm);
  ABmp := TBitmap.Create;
  try
    GenerateFormImageExt(ACallingForm, ABmp);
    Bitmap.Assign(ABmp);
  finally
    FreeAndNil(ABmp);
  end;
  PaintBox1.SetBounds(0, 0, ACallingForm.Width, ACallingForm.Height);
  {$IFDEF XE10_OR_NEWER}
  SetBounds(ACallingForm.Bounds);
  {$ELSE}
  SetBounds(ACallingForm.Left, ACallingForm.Top, ACallingForm.Width, ACallingForm.Height);
  {$ENDIF}


  Visible := True;
end;

procedure TfrmSlideMenuUI.PaintBox1Click(Sender: TObject);
begin
  CloseMenu;
  FCallingForm.Visible := True;
  //SelectItem(Self, ksVirtualListView1.Items[ksVirtualListView1.ItemIndex]);

end;

procedure TfrmSlideMenuUI.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.DrawBitmap(FBitmap, RectF(0, 0, FBitmap.Width, FBitmap.Height), ClientRect, 1, False);
end;

procedure TfrmSlideMenuUI.SelectItem(Sender: TObject; AItem: TksVListItem);
begin
  if FCanSelect = False then
    Exit;
  FCanSelect := False;
  if Assigned(FOnSelectItem) then
    FOnSelectItem(Self, AItem);
end;

end.
