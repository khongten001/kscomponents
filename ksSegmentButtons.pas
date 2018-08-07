{*******************************************************************************
*                                                                              *
*  TksSegmentButtons - Segment button selection component                      *
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

unit ksSegmentButtons;

interface

{$I ksComponents.inc}

uses
  System.Classes, FMX.Types, FMX.Controls, FMX.Graphics, System.Types, System.UITypes,
  FMX.StdCtrls, System.Generics.Collections, FMX.Objects, FMX.Effects,
  System.UIConsts, ksSpeedButton, ksTypes,FMX.ImgList,FMX.ActnList,System.ImageList,fmx.utils,

  System.Actions,FMX.MultiResBitmap, FMX.InertialMovement, FMX.BehaviorManager;


type
  TksSegmentButton = class;
  TksSegmentButtons = class;

  TksSelectSegmentButtonEvent = procedure(Sender: TObject; AIndex: integer; AButton: TksSegmentButton) of object;


  TksSegmentSpeedButton = class(TControl)
  protected

    FBadge: integer;
    FIsPressed: Boolean;
    FText: string;
    FIndex: integer;
    FImageIndex: Integer;
    FOwner: TksSegmentButtons;
    FIcon: TksStandardIcon;
    FBitmap: TBitmap;
    FImages: TImageList;
    function GetImages: TImageList;
    procedure SetImages(const Value: TImageList);

    procedure Changed;
    procedure SetBadge(const Value: integer);
    procedure SetIcon(const Value: TksStandardIcon);
    procedure SetIsPressed(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure ChooseImage;
    protected
    procedure Paint; override;
    //procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    function GetImageIndex: Integer;
    procedure SetImageIndex(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Badge: integer read FBadge write SetBadge;
    property IsPressed: Boolean read FIsPressed write SetIsPressed;
    property Text: string read FText write SetText;
    property Icon: TksStandardIcon read FIcon write SetIcon;
    property Images: TImageList read GetImages write setimages;

    property Index: integer read FIndex write FIndex;
    property ImageIndex: Integer (* TImageIndex *) read GetImageIndex write SetImageIndex  default -1;

  end;

  TKsSegmentButton = class(TCollectionItem)
  protected

    function ImageIndexStored: Boolean; virtual;
   private
    FID: string;
    FText: string;
    FVisible: Boolean;
    FIcon: TksStandardIcon;
    FimageIndex:Integer;
    function GetImageIndex: Integer;
    procedure SetImageIndex(const Value: Integer);

    procedure SetText(const Value: string);
    function GetBadgeValue: integer;
    procedure SetBadgeValue(const Value: integer);
    function GetBoundsRect: TRectF;
    function GetIndex: integer;
    procedure SetVisible(const Value: Boolean);
    procedure SetIcon(const Value: TksStandardIcon);
    public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

  public
     FButton: TksSegmentSpeedButton;

  published
    property ID: string read FID write FID;
    property Text: string read FText write SetText;
    property BoundsRect: TRectF read GetBoundsRect;
    property Index: integer read GetIndex;
    property BadgeValue: integer read GetBadgeValue write SetBadgeValue;
    property Icon: TksStandardIcon read FIcon write SetIcon;
    property Visible: Boolean read FVisible write SetVisible default True;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex stored ImageIndexStored default -1;

  end;

  TksSegmentButtonCollection = class(TCollection)
  private
    FSegmentButtons: TKsSegmentButtons;
  protected
    function GetOwner: TPersistent; override;
    function GetItem(Index: Integer): TKsSegmentButton;
    procedure SetItem(Index: Integer; Value: TKsSegmentButton);
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AButtons: TKsSegmentButtons);
    function Add: TKsSegmentButton;
    function Insert( Index: Integer ): TKsSegmentButton;
    function VisibleCount: integer;
    property Items[index: Integer]: TKsSegmentButton read GetItem write SetItem; default; // default - Added by Fenistil
  end;


  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64
    {$ELSE} pidiOSDevice {$ENDIF} or pidiOSSimulator or pidAndroid)]
  TksSegmentButtons = class(TksControl)
  private
    FGroupID: string;
    FInitialIndex: integer;
    FItemIndex: integer;
    FBtnSize: single;
    FFontSize: integer;
    FOnChange: TNotifyEvent;
    FSegments: TksSegmentButtonCollection;
    FTintColor: TAlphaColor;
    FBackgroundColor: TAlphaColor;
    FOnSelectSegment: TksSelectSegmentButtonEvent;
    FChanged: Boolean;
    FMouseUpCalled: Boolean;
    FVertical: Boolean;
    FImages: TImageList;
    FInternalMargin:Integer;
    procedure UpdateButtons;
    procedure SetItemIndex(const Value: integer);
    procedure SetSegments(const Value: TksSegmentButtonCollection);
    procedure SetBackgroundColor(const Value: TAlphaColor);
    procedure SetTintColor(const Value: TAlphaColor);
    function GetSelected: TKsSegmentButton;
    function GetSelectedID: string;
    procedure SetSelectedID(const Value: string);
    function ButtonFromPos(x,y: single): TksSegmentButton;
    procedure SetFontSize(const Value: integer);
    procedure SetVertical(const Value: Boolean);
    function GetImages: TImageList;
    procedure SetImages(const Value: TImageList);
    function getInternalMargin: Integer;
    procedure setInternalMargin(const Value: Integer);


   protected
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
    procedure Paint; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SelectSegmentByText(AText: string);
    property Selected: TKsSegmentButton read GetSelected;
    property SelectedID: string read GetSelectedID write SetSelectedID;
  published
    property Align;
    property FontSize: integer read FFontSize write SetFontSize default 14;
    property ItemIndex: integer read FItemIndex write SetItemIndex default -1;
    property Margins;
    property Padding;
    property Position;
    property Width;
    property Images: TImageList read GetImages write SetImages;
    Property InternalMargin:Integer read getInternalMargin write setInternalMargin default 8;
    property TintColor: TAlphaColor read FTintColor write SetTintColor default claNull;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default claNull;
    property Segments: TksSegmentButtonCollection read FSegments write SetSegments;
    property Vertical:Boolean read FVertical write SetVertical default false;
    property Size;
    property Height;
    property Visible;
    // events
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseDown;
    property OnMouseUp;
    property OnSelectSegment: TksSelectSegmentButtonEvent read FOnSelectSegment write FOnSelectSegment;


  end;

  {$R *.dcr}

  procedure Register;

  {}


implementation

uses SysUtils,  Math, ksCommon, FMX.Forms,System.TypInfo;



procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksSegmentButtons]);
end;

{ TKsSegmentButton }
function TKsSegmentButton.ImageIndexStored: Boolean;
begin
  Result :=  (ImageIndex <> -1);
end;


procedure TKsSegmentButton.Assign(Source: TPersistent);
begin
  if (Source is TKsSegmentButton) then
  begin
    FID := (Source as TKsSegmentButton).ID;
    FText := (Source as TKsSegmentButton).Text;
  end;
end;


constructor TKsSegmentButton.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FimageIndex:=-1;
  FButton := TksSegmentSpeedButton.Create((Collection as TksSegmentButtonCollection).FSegmentButtons);
  FVisible := True;
  ficon:=TksStandardIcon.Custom;


end;

destructor TKsSegmentButton.Destroy;
begin
  {$IFDEF NEXTGEN}
  FButton.DisposeOf;
  {$ELSE}
  FButton.Free;
  {$ENDIF}
  inherited;
end;

function TKsSegmentButton.GetBadgeValue: integer;
begin
  Result := FButton.Badge;
end;

function TKsSegmentButton.GetBoundsRect: TRectF;
begin
  Result := FButton.BoundsRect;
end;


function TKsSegmentButton.GetImageIndex: Integer;
begin
  result:=FimageIndex;
end;

function TKsSegmentButton.GetIndex: integer;
begin
  Result := FButton.Index;
end;




procedure TKsSegmentButton.SetBadgeValue(const Value: integer);
begin
  FButton.Badge := Value;
end;

procedure TKsSegmentButton.SetIcon(const Value: TksStandardIcon);
begin
  FIcon := Value;
  (Collection as TksSegmentButtonCollection).FSegmentButtons.UpdateButtons;

end;



procedure TKsSegmentButton.SetImageIndex(const Value: Integer);
begin
 FimageIndex:=value;
 if fbutton<>nil then
  fbutton.setImageIndex(value);
end;

procedure TKsSegmentButton.SetText(const Value: string);
begin
  FText := Value;
  (Collection as TksSegmentButtonCollection).FSegmentButtons.UpdateButtons;
end;



procedure TKsSegmentButton.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    (Collection as TksSegmentButtonCollection).FSegmentButtons.UpdateButtons;
  end;
end;

{procedure TKsSegmentButton.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

 TksSegmentButtons }


procedure TksSegmentButtons.Assign(Source: TPersistent);
begin
  if (Source is TksSegmentButtons) then
  begin
    FGroupID := (Source as TksSegmentButtons).FGroupID;
    FItemIndex := (Source as TksSegmentButtons).FItemIndex;
    FBtnSize := (Source as TksSegmentButtons).FBtnSize;
    FVertical:= (Source as TksSegmentButtons).FVertical;
    FOnChange := (Source as TksSegmentButtons).FOnChange;
    FTintColor := (Source as TksSegmentButtons).FTintColor;
    FImages := (Source as TksSegmentButtons).FImages;
    fimages:=(Source as TksSegmentButtons).fimages;
    FInternalMargin:=(Source as TksSegmentButtons).FInternalMargin;
    FBackgroundColor := (Source as TksSegmentButtons).FBackgroundColor;
    FSegments.Assign((Source as TksSegmentButtons).Segments);
  end;
end;

function TksSegmentButtons.ButtonFromPos(x, y: single): TksSegmentButton;
var
  ICount: integer;
  ABtn: TksSegmentButton;
begin
  Result := nil;
  for ICount := 0 to FSegments.Count-1 do
  begin
    ABtn := FSegments.Items[ICount] as TksSegmentButton;
    if PtInRect(ABtn.BoundsRect, PointF(x, y)) then
    begin
      Result := ABtn;
      Exit;
    end;
  end;
end;

constructor TksSegmentButtons.Create(AOwner: TComponent);
var
  AGuid: TGUID;
begin
  inherited;
  Fimages:=nil;
  FinternalMargin:=8;
  FSegments := TksSegmentButtonCollection.Create(Self);
  SetAcceptsControls(False);
  CreateGUID(AGuid);
  FGroupID := AGuid.ToString;
  FGroupID := StringReplace(FGroupID, '{', '', [rfReplaceAll]);
  FGroupID := StringReplace(FGroupID, '-', '', [rfReplaceAll]);
  FGroupID := StringReplace(FGroupID, '}', '', [rfReplaceAll]);
  FBackgroundColor := claNull;
  FTintColor := claNull;
  fVertical :=false;
  Size.Height := 50;
  Size.Width := 300;
  FFontSize := 14;
  FChanged := False;

end;

destructor TksSegmentButtons.Destroy;
begin
  FreeAndNil(FSegments);
  inherited;
end;



procedure TksSegmentButtons.DoMouseLeave;
begin
  inherited;
  if FMouseUpCalled then
    Exit;
  if FInitialIndex <> FItemIndex then
  begin
    FItemIndex := FInitialIndex;
    FChanged := False;
    UpdateButtons;

     {
    if Assigned(FOnSelectSegment) then
      FOnSelectSegment(Self, FItemIndex, Segments[FItemIndex]); }

  end;
end;

function TksSegmentButtons.GetSelected: TKsSegmentButton;
begin
  Result := nil;
  if (FItemIndex > -1) and (FItemIndex < FSegments.Count) then
    Result := FSegments[FItemIndex];
end;

function TksSegmentButtons.GetSelectedID: string;
begin
  Result := '';
  if Selected <> nil then
    Result := Selected.ID;
end;

procedure TksSegmentButtons.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  ASegment: TksSegmentButton;
begin
  inherited;
  FMouseUpCalled := False;
  FInitialIndex := FItemIndex;
  HitTest := False;
  try
    FChanged := False;
    ASegment := ButtonFromPos(x, y);
    if ASegment <> nil then
    begin
      if ASegment.Index <> FItemIndex then
      begin
        ItemIndex := ASegment.Index;

      end;
    end;
    //Application.ProcessMessages;
  finally
    HitTest := True;
  end;
end;

procedure TksSegmentButtons.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  FMouseUpCalled := True;
  if FChanged then
  begin
    if Assigned(FOnSelectSegment) then
      FOnSelectSegment(Self, FItemIndex, Segments[FItemIndex]);
  end;
end;

{
procedure TksSegmentButtons.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  ItemIndex := Min(Trunc(X / FBtnWidth), Segments.Count-1);
  FMouseDown := True;
end;  }
        (*
procedure TksSegmentButtons.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  {$IFDEF MSWINDOWS}
  if FMouseDown then
  begin
    Tap(PointF(x, y));
  end;
  {$ENDIF}
  FMouseDown := False;
  inherited;
end;    *)

procedure TksSegmentButtons.Paint;
begin
  inherited;
  if Locked then
    Exit;
  if (csDesigning in ComponentState) then
  begin
    DrawDesignBorder(claDimgray, claDimgray);
    if FSegments.Count = 0 then
      Canvas.FillText(ClipRect, 'Add "Segments" in the Object Inspector', True, 1, [], TTextAlign.Center);
  end;
end;

procedure TksSegmentButtons.Resize;
begin
  inherited;
  UpdateButtons;
end;

procedure TksSegmentButtons.UpdateButtons;
var
  ICount: integer;
  i: integer;
begin
  if FSegments.VisibleCount = 0 then
    Exit;
  if not fvertical then
   begin
    FBtnSize := (Width-finternalmargin*2) / FSegments.VisibleCount;
    i := 0;
    for ICount := 0 to FSegments.Count-1 do
    begin
      FSegments[ICount].FButton.Visible := FSegments[ICount].Visible;
      if FSegments[Icount].Visible then
      begin
        if Assigned(FSegments[ICount].FButton) then
        begin
          if ContainsObject(FSegments[ICount].FButton) = False then
            AddObject(FSegments[ICount].FButton);

          with FSegments[ICount].FButton do
          begin
           (* IsPressed := False;

            if ICount = 0 then s := 'toolbuttonleft';
            if ICount > 0 then s := 'toolbuttonmiddle';
            if ICount = FSegments.Count-1 then s := 'toolbuttonright';


            {$IFDEF ANDROID}
            //StyleLookup := 'listitembutton';
            //Height := 30;
            {$ELSE}
            //FSegments[ICount].FButton.StyleLookup := s;

            //StaysPressed := ICount = FItemIndex;

            //GroupName := FGroupID;

            //TintColor := FTintColor;

                                  *)
            Index := ICount;

            if Selected <> nil then
            begin
              if (Selected.Visible = False) and (FSegments.VisibleCount > 0) then
                ItemIndex := ICount;
            end;

            IsPressed := ICount = FItemIndex;
            Width := FBtnSize;
            Height := self.Height-(FInternalMargin*2);

            {TextSettings.FontColorForState.Focused := FTintColor;
            TextSettings.FontColorForState.Active := FTintColor;
            TextSettings.FontColorForState.Normal := FTintColor;
            TextSettings.FontColorForState.Pressed := FBackgroundColor;}
            Text := FSegments[ICount].Text;
            Icon:= FSegments[ICount].Icon;
            //TextSettings.FontColor := FTintColor;

           // {$ENDIF}
            Position.Y := (Self.Height - Height) / 2;
            Position.X := (i * FBtnSize)+FInternalMargin;

            i := i + 1;
          end;
        end;
      end;
    end;
   end
  else
   begin
     FBtnSize := (Height-finternalmargin*2) / FSegments.VisibleCount;
    i := 0;
    for ICount := 0 to FSegments.Count-1 do
    begin
      FSegments[ICount].FButton.Visible := FSegments[ICount].Visible;
      if FSegments[Icount].Visible then
      begin
        if Assigned(FSegments[ICount].FButton) then
        begin
          if ContainsObject(FSegments[ICount].FButton) = False then
            AddObject(FSegments[ICount].FButton);

          with FSegments[ICount].FButton do
          begin
            Index := ICount;
            if Selected <> nil then
            begin
              if (Selected.Visible = False) and (FSegments.VisibleCount > 0) then
                ItemIndex := ICount;
            end;

            IsPressed := ICount = FItemIndex;
            Width := self.Width-(finternalmargin*2);
            Height := FBtnSize;

            {TextSettings.FontColorForState.Focused := FTintColor;
            TextSettings.FontColorForState.Active := FTintColor;
            TextSettings.FontColorForState.Normal := FTintColor;
            TextSettings.FontColorForState.Pressed := FBackgroundColor;}
            Text := FSegments[ICount].Text;
            Icon:= FSegments[ICount].Icon;

            //TextSettings.FontColor := FTintColor;

           // {$ENDIF}
            Position.Y := (i * FBtnSize)+FInternalMargin;
            Position.X :=(Self.width - width) / 2;

            i := i + 1;
          end;
        end;
      end;
    end;
   end;

end;

procedure TksSegmentButtons.SelectSegmentByText(AText: string);
var
  ICount: integer;
  ASeg: TKsSegmentButton;
begin
  for ICount := 0 to FSegments.Count-1 do
  begin
    ASeg := FSegments.Items[ICount];
    if ASeg.Text = AText then
    begin
      ItemIndex := ICount;
      Exit;
    end;
  end;
end;

procedure TksSegmentButtons.SetBackgroundColor(const Value: TAlphaColor);
begin
  FBackgroundColor := Value;
  UpdateButtons;
end;


procedure TksSegmentButtons.SetFontSize(const Value: integer);
begin
  FFontSize := Value;
  UpdateButtons;
end;


procedure TksSegmentButtons.SetItemIndex(const Value: integer);
begin
  if FItemIndex <> Value then
  begin
    FChanged := True;
    FItemIndex := Value;
    UpdateButtons;
    Repaint;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TksSegmentButtons.SetSegments(const Value: TksSegmentButtonCollection);
begin
  FSegments.Assign(Value);
end;

procedure TksSegmentButtons.SetSelectedID(const Value: string);
var
  ICount: integer;
begin
  for ICount := 0 to Segments.Count-1 do
  begin
    if Segments[ICount].ID = Value then
    begin
      ItemIndex := ICount;
      Exit;
    end;
  end;
  ItemIndex := -1;
end;

procedure TksSegmentButtons.SetTintColor(const Value: TAlphaColor);
begin
  FTintColor := Value;
  UpdateButtons;
end;

procedure TksSegmentButtons.SetVertical(const Value: Boolean);
var t:single;
begin
  if fvertical<>value then
   begin
    FVertical := Value;
    t:=Size.Height;
    Size.Height := Size.Width;
    Size.Width := t;
   end;
end;

{ TksSegmentButtonCollection }

function TksSegmentButtonCollection.Add: TKsSegmentButton;
begin
  Result := inherited Add as TKsSegmentButton;
  FSegmentButtons.UpdateButtons;
end;

constructor TksSegmentButtonCollection.Create(AButtons: TKsSegmentButtons);
begin
  inherited Create(TKsSegmentButton);
  FSegmentButtons := AButtons;
end;

function TksSegmentButtonCollection.GetItem(Index: Integer): TKsSegmentButton;
begin
  Result := inherited Items[index] as TKsSegmentButton;
end;

function TksSegmentButtonCollection.GetOwner: TPersistent;
begin
  Result := FSegmentButtons;
end;

function TksSegmentButtonCollection.Insert(Index: Integer): TKsSegmentButton;
begin
  Result := inherited insert( index ) as TKsSegmentButton;
  FSegmentButtons.UpdateButtons;
end;

procedure TksSegmentButtonCollection.SetItem(Index: Integer; Value: TKsSegmentButton);
begin
  inherited SetItem(index, Value);
end;

procedure TksSegmentButtonCollection.Update(Item: TCollectionItem);
begin
  inherited;
  (Owner as TksSegmentButtons).UpdateButtons;
end;

function TksSegmentButtonCollection.VisibleCount: integer;
var
  ICount: integer;
begin
  Result := 0;
  for ICount := 0 to Count-1 do
  begin
    if Items[ICount].Visible then
      Result := Result + 1;
  end;
end;

{ TksSegmentSpeedButton }



function TksSegmentSpeedButton.GetImageIndex: Integer (* TImageIndex *);
begin
  Result := FImageIndex;
end;

procedure TksSegmentSpeedButton.SetImageIndex(const Value: Integer (* TImageIndex *));
begin
  if FImageIndex<>value then
   begin
     FImageIndex:=value;
     ChooseImage;
   end;
end;


function TksSegmentSpeedButton.GetImages: TImageList;
begin
  result:=Fimages;
end;

procedure TksSegmentSpeedButton.SetImages(const Value: TImageList);
begin
  if (Fimages<>value) then
  begin
    FImages:=value;
    ChooseImage;
  end;

end;




procedure TksSegmentSpeedButton.Changed;
begin
  Repaint;
end;

constructor TksSegmentSpeedButton.Create(AOwner: TComponent);
begin
  inherited;
  FOwner := (AOwner as TksSegmentButtons);
  Fimageindex:=-1;
  FBitmap := TBitmap.Create;
  Stored := False;
  HitTest := False;
  ficon:=TksStandardIcon.Custom;
end;

destructor TksSegmentSpeedButton.Destroy;
begin
  FreeAndNil(FBitmap);

  inherited;
end;

(*
procedure TksSegmentSpeedButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;
  //if HitTest = False then
  //  Exit;

  {if FOwner.ItemIndex <> FIndex then
  begin
    HitTest := False;
    Application.ProcessMessages;
    FOwner.ItemIndex := FIndex;
    FOwner.DoSelectSegment;
    HitTest := True;
  end;  }
end;    *)

procedure TksSegmentSpeedButton.Paint;
var
 ABmp: TBitmap;

 AImageRect: TRectF;
begin
  inherited;
  if Locked then
    Exit;

  Canvas.Stroke.Color := claBlack;

  if FIsPressed then
    Canvas.Fill.Color := GetColorOrDefault(FOwner.TintColor, claDodgerblue)
  else
    Canvas.Fill.Color := FOwner.BackgroundColor;

  Canvas.Stroke.Color := GetColorOrDefault(FOwner.TintColor, claDodgerblue);
  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.FillRect(ClipRect, 0, 0, AllCorners, 1);
  Canvas.DrawRect(ClipRect, 0, 0, AllCorners, 1);

  if FIsPressed then
    Canvas.Fill.Color := claWhite
  else
    Canvas.Fill.Color := GetColorOrDefault(FOwner.TintColor, claDodgerblue);

  Canvas.Font.Size := FOwner.FontSize;
  //ShowMessage(fowner.FontSize.ToString);
  RenderText(Canvas, ClipRect, FText, Canvas.Font, Canvas.Fill.Color, False, TTextAlign.Center, TTextAlign.Center, TTextTrimming.Character);

  if FBadge > 0 then
    GenerateBadge(Canvas, PointF(ClipRect.Right-20, ClipRect.Top-6), FBadge, claRed, claWhite);


    ABmp := TBitmap.Create;
    try

      ABmp.Assign(FBitmap);
      if  FisPressed then
       begin
       ReplaceOpaqueColor(ABmp, claWhite);
       end
       else
      begin
        {$IFDEF IOS}
        ReplaceOpaqueColor(ABmp, GetColorOrDefault(FOwner.TintColor, claDodgerblue));
        {$ENDIF}
        {$IFDEF ANDROID}
        ReplaceOpaqueColor(ABmp, GetColorOrDefault(FOwner.TintColor, claDimgray));
        {$ENDIF}
        {$IFDEF MSWINDOWS}
        ReplaceOpaqueColor(ABmp, GetColorOrDefault(FOwner.TintColor, claDodgerblue));
        {$ENDIF}
      end;
         AImageRect := RectF(0, 0, 24, 24);;
        OffsetRect(AImageRect,
                (Width - AImageRect.Width) / 2,
                (Height - AImageRect.Height) / 2);
             Canvas.DrawBitmap(ABmp,
                        RectF(0, 0, ABmp.Width, ABmp.Height),
                        AImageRect,
                        1,
                        False);


    finally
      ABmp.Free;
    end;

end;


procedure TksSegmentSpeedButton.SetIcon(const Value: TksStandardIcon);
begin
 if Value <> Ficon  then
  begin
   ficon:=value;
   ChooseImage;
  end;
end;


procedure TksSegmentSpeedButton.ChooseImage;
var
  AStream: TResourceStream;
  AEnumName: String;
begin
  if ficon <> TksStandardIcon.Custom  then
  begin
    AEnumName := GetENumName(TypeInfo(TksStandardIcon), Ord(Ficon));
    AStream := TResourceStream.Create(HInstance, AEnumName, RT_RCDATA);
    try
      FBitmap.Clear(claNull);
      FBitmap.LoadFromStream(AStream);

      (* if (FIsPressed) then
      begin
        ReplaceOpaqueColor(FBitmap, claLightskyblue);
      end
      else
      begin
        ReplaceOpaqueColor(FBitmap, claDodgerblue);
      end; *)
    finally
      AStream.Free;
    end;
  end
 else
  begin
   FBitmap.Clear(claNull);
   if fimages<>nil then
    begin
      if (FImageIndex>=0) and (FImageIndex<Fimages.Count) then
       begin
        FBitmap.Assign(Fimages.Destination[FimageIndex].Layers[0].MultiResBitmap.ItemByScale(1, False, False).Bitmap);
        // ABmp := FMainMenu.Images.Source[AImageIndex].MultiResBitmap.ItemByScale(1, False, False);;

       end;
    end;
  end;

  Changed;
end;


procedure TksSegmentSpeedButton.SetBadge(const Value: integer);
begin
  FBadge := Value;
  Changed;
end;

procedure TksSegmentSpeedButton.SetIsPressed(const Value: Boolean);
begin
  FIsPressed := Value;
  Changed;
end;

procedure TksSegmentSpeedButton.SetText(const Value: string);
begin
  FText := Value;
  Changed;
end;




(* function TksSegmentButtons.GetImageList: TBaseImageList;
begin
  Result := GetImages;
end;

procedure TksSegmentButtons.SetImageList(const Value: TBaseImageList);
begin
  ValidateInheritance(Value, TImageList);
  SetImages(TImageList(Value));
end; *)

function TksSegmentButtons.GetImages: TImageList;
begin
  Result := FImages;
end;

function TksSegmentButtons.getInternalMargin: Integer;
begin
 result:=FInternalMargin;
end;

procedure TksSegmentButtons.SetImages(const Value: TImageList);
var i:integer;
begin
  if FImages <> Value then
  begin
    FImages := value;
    for I := 0 to Segments.Count - 1 do
      segments[i].fbutton.Images:=Value;
  end;
end;


procedure TksSegmentButtons.setInternalMargin(const Value: Integer);
begin
 if value<>FInternalMargin then
  begin
    FInternalMargin:=value;
    UpdateButtons;
  end;
end;

end.

