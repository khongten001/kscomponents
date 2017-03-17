{*******************************************************************************
*                                                                              *
*  TksSlideMenu - Slide Menu Component                                         *
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

unit ksSlideMenu;

interface

{$I ksComponents.inc}

//{$DEFINE ADD_SAMPLE_MENU_ITEMS}


uses System.UITypes, FMX.Controls, FMX.Layouts, FMX.Objects, System.Classes,
  FMX.Types, Generics.Collections, FMX.Graphics, System.UIConsts, FMX.Effects,
  FMX.StdCtrls, System.Types, FMX.Forms, {ksTableView,} ksVirtualListView, ksTypes, ksSlideMenuUI
  {$IFDEF XE8_OR_NEWER}
  ,FMX.ImgList
  {$ENDIF}
  ;

const
  C_DEFAULT_MENU_WIDTH = 220;
  C_DEFAULT_MENU_TOOLBAR_HEIGHT = 44;

  C_DEFAULT_MENU_HEADER_HEIGHT = 30;
  C_DEFAULT_MENU_HEADER_FONT_SIZE = 16;
  C_DEFAULT_MENU_HEADER_TEXT_COLOR = claWhite;
  C_DEFAULT_MENU_HEADER_COLOR = $FF323232;

  C_DEFAULT_MENU_ITEM_HEIGHT = 50;
  C_DEFAULT_MENU_FONT_SIZE = 14;
  C_DEFAULT_MENU_TOOLBAR_FONT_SIZE = 14;
  C_DEFAULT_MENU_SLIDE_SPEED = 0.15;

  C_DEFAULT_MENU_SELECTED_COLOR = claWhite;
  C_DEFAULT_MENU_SELECTED_FONT_COLOR = claWhite;
  C_DEFAULT_MENU_FONT_COLOR = claBlack;
  C_DEFAULT_MENU_BACKGROUND_COLOR = claWhite;
  C_DEFAULT_MENU_TOOLBAR_COLOR = claWhite;

type
  TksSlideMenu = class;
  TksMenuPosition = (mpLeft, mpRight);
  TKsMenuStyle = (msOverlap, msReveal);
  TKsMenuTheme = (mtCustom, mtDarkGray, mtDarkBlue, mtDarkOrange, mtDarkGreen, mtLightGray, mtLightBlue, mtLightOrange, mtLightGreen);

  TSelectMenuItemEvent = procedure(Sender: TObject; AId: string) of object;

  TksSlideMenuAppearence = class(TPersistent)
  private
    [weak]FSlideMenu: TksSlideMenu;
    FBackgroundColor: TAlphaColor;
    FHeaderColor: TAlphaColor;
    FHeaderFontColor: TAlphaColor;
    FItemColor: TAlphaColor;
    FFontColor: TAlphaColor;
    FSelectedColor: TAlphaColor;
    FSelectedFontColor: TAlphaColor;
    FTheme: TKsMenuTheme;
    FToolBarColor: TAlphaColor;
    FAccessoryColor: TAlphaColor;
    procedure SetHeaderColor(const Value: TAlphaColor);
    procedure SetHeaderFontColor(const Value: TAlphaColor);
    procedure SetItemColor(const Value: TAlphaColor);
    procedure SetFontColor(const Value: TAlphaColor);
    procedure SetSelectedColor(const Value: TAlphaColor);
    procedure SetSelectedFontColor(const Value: TAlphaColor);
    procedure SetTheme(const Value: TKsMenuTheme);
    procedure SetToolBarColor(const Value: TAlphaColor);
    procedure SetAccessoryColor(const Value: TAlphaColor);
    procedure SetBackgroundColor(const Value: TAlphaColor);
  public
    constructor Create(ASlideMenu: TksSlideMenu); virtual;
  published
    property AccessoryColor: TAlphaColor read FAccessoryColor write SetAccessoryColor default claWhite;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default $FF222222;
    property HeaderColor: TAlphaColor read FHeaderColor write SetHeaderColor default C_DEFAULT_MENU_HEADER_COLOR;
    property HeaderFontColor: TAlphaColor read FHeaderFontColor write SetHeaderFontColor default C_DEFAULT_MENU_HEADER_TEXT_COLOR;
    property ItemColor: TAlphaColor read FItemColor write SetItemColor default $FF222222;
    property FontColor: TAlphaColor read FFontColor write SetFontColor default claWhite;
    property SelectedItemColor: TAlphaColor read FSelectedColor write SetSelectedColor default claRed;
    property SelectedFontColor: TAlphaColor read FSelectedFontColor write SetSelectedFontColor default claWhite;
    property ToolBarColor: TAlphaColor read FToolBarColor write SetToolBarColor default $FF323232;
    property Theme: TKsMenuTheme read FTheme write SetTheme default mtDarkGray;
  end;

  TksSlideMenuItem = class
    FID: string;
    FText: string;
    FForm: TCommonCustomForm;
    FIcon: TksStandardIcon;
  end;

  TksSlideMenuItemList = class(TObjectList<TksSlideMenuItem>)
  public
    procedure AddItem(AID, AText: string; AForm: TCommonCustomForm; AIcon: TksStandardIcon);
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64
    {$ELSE} pidiOSDevice {$ENDIF} or pidiOSSimulator or pidAndroid)]

  TksSlideMenu = class(TComponent)
  private
    FItems: TksSlideMenuItemList;
    FCallingForm: TCommonCustomForm;
    FMenuForm: TfrmSlideMenuUI;
    FAppearence: TksSlideMenuAppearence;
    FOnSelectMenuItemEvent: TSelectMenuItemEvent;
    FAfterSelectMenuItemEvent: TSelectMenuItemEvent;
    procedure RebuildMenu;
    procedure SelectItem(Sender: TObject; AItem: TksVListItem);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddMenuItem(AID, AText: string; const AForm: TCommonCustomForm = nil; const AIcon: TksStandardIcon = Custom);
    procedure OpenMenu(ACallingForm: TCommonCustomForm);
  published
    property Appearence: TksSlideMenuAppearence read FAppearence write FAppearence;
    property OnSelectMenuItemEvent: TSelectMenuItemEvent read FOnSelectMenuItemEvent write FOnSelectMenuItemEvent;
    property AfterSelectItemEvent: TSelectMenuItemEvent read FAfterSelectMenuItemEvent write FAfterSelectMenuItemEvent;
  end;

  //{$R *.dcr}

  procedure Register;



implementation

uses SysUtils, ksCommon, System.TypInfo;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksSlideMenu]);
end;


{ TksSlideMenuAppearence }

constructor TksSlideMenuAppearence.Create(ASlideMenu: TksSlideMenu);
begin
  inherited Create;
  FSlideMenu := ASlideMenu;
  FHeaderColor := $FF323232;
  FItemColor := $FF222222;
  FBackgroundColor := $FF222222;;
  FToolBarColor := $FF323232;
  FFontColor := claWhite;
  FSelectedFontColor := claWhite;
  FSelectedColor := claRed;
  FAccessoryColor := claWhite;
  FTheme := mtDarkGray;
end;

procedure TksSlideMenuAppearence.SetAccessoryColor(const Value: TAlphaColor);
begin
  FAccessoryColor := Value;
end;

procedure TksSlideMenuAppearence.SetBackgroundColor(const Value: TAlphaColor);
begin
  FBackgroundColor := Value;
end;

procedure TksSlideMenuAppearence.SetFontColor(const Value: TAlphaColor);
begin
  FFontColor := Value;
  FTheme := mtCustom;

end;

procedure TksSlideMenuAppearence.SetHeaderColor(const Value: TAlphaColor);
begin
  FHeaderColor := Value;
  FTheme := mtCustom;
end;



procedure TksSlideMenuAppearence.SetHeaderFontColor(const Value: TAlphaColor);
begin
  FHeaderFontColor := Value;
  FTheme := mtCustom;
end;

procedure TksSlideMenuAppearence.SetItemColor(const Value: TAlphaColor);
begin
  FItemColor := Value;
  FTheme := mtCustom;
end;

procedure TksSlideMenuAppearence.SetSelectedColor(const Value: TAlphaColor);
begin
  FSelectedColor := Value;
  FTheme := mtCustom;
end;

procedure TksSlideMenuAppearence.SetSelectedFontColor(const Value: TAlphaColor);
begin
  FSelectedFontColor := Value;
  FTheme := mtCustom;
end;

procedure TksSlideMenuAppearence.SetTheme(const Value: TKsMenuTheme);
begin
   if Value = mtDarkGray then
    begin
      FHeaderColor := $FF424242;
      FToolBarColor := $FF323232;
      FItemColor := $FF545454;
      FBackgroundColor := FItemColor;
      FFontColor := claWhite;
      FHeaderFontColor := $FFDADADA;
      FSelectedFontColor := claWhite;
      FSelectedColor := claRed;
    end;
  if Value = mtDarkBlue then
    begin
      FHeaderColor := $FF2A7A9D;
      FToolBarColor := $FF323232;
      FItemColor := $FF2A7A9D;
      FBackgroundColor := FItemColor;
      FFontColor := claWhite;
      FHeaderFontColor := $FFC7FFFB;
      FSelectedFontColor := claWhite;
      FSelectedColor := $FF1A5670;
    end;
  if Value = mtDarkOrange then
    begin
      FHeaderColor := $FFFF9900;
      FToolBarColor := $FF323232;
      FItemColor := $FFBC7202;
      FBackgroundColor := FItemColor;
      FFontColor := claWhite;
      FHeaderFontColor := claBlack;
      FSelectedFontColor := claWhite;
      FSelectedColor := $FFBC7202;
    end;
  if Value = mtDarkGreen then
    begin
      FHeaderColor := $FF76D015;
      FToolBarColor := $FF323232;
      FItemColor := $FF424242;
      FBackgroundColor := FItemColor;
      FFontColor := claWhite;
      FHeaderFontColor := claBlack;
      FSelectedFontColor := claBlack;
      FSelectedColor := $FFDCFF00;
    end;
  if Value = mtLightGray then
    begin
      FHeaderColor := $FF424242;
      FToolBarColor := $FF323232;
      FItemColor := $FF828282;
      FBackgroundColor := FItemColor;
      FFontColor := claWhite;
      FHeaderFontColor := $FFDADADA;
      FSelectedFontColor := claWhite;
      FSelectedColor := claRed;
    end;
  if Value = mtLightBlue then
    begin
      FHeaderColor := $FF424242;
      FToolBarColor := $FF323232;
      FItemColor := $FF2A7A9D;
      FBackgroundColor := FItemColor;
      FFontColor := claWhite;
      FHeaderFontColor := $FFDADADA;
      FSelectedFontColor := claBlack;
      FSelectedColor := $FFC7FFFB;
    end;
  if Value = mtLightOrange then
    begin
      FHeaderColor := $FF424242;
      FToolBarColor := $FF323232;
      FItemColor := $FFFF9900;
      FBackgroundColor := FItemColor;
      FFontColor := claBlack;
      FHeaderFontColor := $FFDADADA;
      FSelectedFontColor := claBlack;
      FSelectedColor := $FFFFCC00;
    end;
  if Value = mtLightGreen then
    begin
      FHeaderColor := $FF424242;
      FToolBarColor := $FF323232;
      FItemColor := $FF76D015;
      FBackgroundColor := FItemColor;
      FFontColor := claBlack;
      FHeaderFontColor := $FFDADADA;
      FSelectedFontColor := claBlack;
      FSelectedColor := $FFDCFF00;
    end;
  FTheme := Value;
end;

procedure TksSlideMenuAppearence.SetToolBarColor(const Value: TAlphaColor);
begin
  FToolBarColor := Value;
  FTheme := mtCustom;
end;

{ TksSlideMenuExt }

procedure TksSlideMenu.AddMenuItem(AID, AText: string; const AForm: TCommonCustomForm = nil; const AIcon: TksStandardIcon = Custom);
begin
  FItems.AddItem(AID, AText, AForm, AIcon);
  RebuildMenu;
end;

constructor TksSlideMenu.Create(AOwner: TComponent);
begin
  inherited;
  FMenuForm := TfrmSlideMenuUI.Create(nil);
  FItems := TksSlideMenuItemList.Create;
  FMenuForm.OnSelectItem := SelectItem;
  FAppearence := TksSlideMenuAppearence.Create(Self);
end;

destructor TksSlideMenu.Destroy;
begin

  FMenuForm.DisposeOf;
  FreeAndNil(FItems);
  FreeAndNil(FAppearence);
  inherited;
end;

procedure TksSlideMenu.OpenMenu(ACallingForm: TCommonCustomForm);
begin
  FCallingForm := ACallingForm;
  FMenuForm.OpenMenu(ACallingForm);

end;

procedure TksSlideMenu.RebuildMenu;
var
  AStream: TResourceStream;
  AEnumName: String;
  ICount: integer;
  lv: TksVirtualListView;
  AItem: TksVListItem;
  ABmp: TBitmap;
begin
  lv := FMenuForm.ksVirtualListView1;

  lv.Items.Clear;
  for ICount := 0 to FItems.Count-1 do
  begin
    AItem := lv.Items.Add(FItems[ICount].FText, '', '', atMore);
    AItem.TagInt := ICount;
    AItem.Title.TextSettings.FontColor := FAppearence.FontColor;
    AItem.Accessory.SetOpaqueColor(FAppearence.AccessoryColor);
    lv.Appearence.SelectedColor := FAppearence.SelectedItemColor;
    lv.Appearence.SelectedFontColor := FAppearence.SelectedFontColor;
    lv.Appearence.ItemBackground := FAppearence.ItemColor;
    lv.Appearence.Background := FAppearence.BackgroundColor;

    aBmp := TBitmap.Create;
    try
      AEnumName := GetENumName(TypeInfo(TksStandardIcon), Ord(FItems[ICount].FIcon));
      if FItems[ICount].FIcon <> Custom then
      begin

        AStream := TResourceStream.Create(HInstance, AEnumName, RT_RCDATA);
        aBmp.LoadFromStream(AStream);
        ReplaceOpaqueColor(ABmp, claWhite);

        AItem.Image.Bitmap := ABmp;
        AItem.Image.Width := 20;
        AItem.Image.Height := 20;

        AStream.Free;
      end;
    finally
      ABmp.Free;
    end;
  end;
end;

procedure TksSlideMenu.SelectItem(Sender: TObject; AItem: TksVListItem);
var
  mi: TksSlideMenuItem;
  AForm: TCommonCustomForm;
begin
  mi := FItems[AItem.TagInt];
  if Assigned(FOnSelectMenuItemEvent) then
    FOnSelectMenuItemEvent(Self, mi.FID);



  AForm := mi.FForm;
  if AForm = nil then
    AForm := FCallingForm;

  AForm.SetBounds(0, 0, FCallingForm.Width, FCallingForm.Height);
  FMenuForm.Image1.Bitmap := GenerateFormImageExt(AForm);
  FMenuForm.CloseMenu;

  TThread.Synchronize (TThread.CurrentThread,
    procedure ()
    begin
      AForm.Visible := True;
      AForm.BringToFront;
      Screen.ActiveForm := AForm;
    end);

  if Assigned(FAfterSelectMenuItemEvent) then
    FAfterSelectMenuItemEvent(Self, mi.FID);
end;

{TksSlideMenuItemExtList }

procedure TksSlideMenuItemList.AddItem(AID, AText: string;
  AForm: TCommonCustomForm; AIcon: TksStandardIcon);
var
  AItem: TksSlideMenuItem;
begin
  AItem := TksSlideMenuItem.Create;
  AItem.FID := AID;
  AItem.FText := AText;
  AItem.FForm := AForm;
  AItem.FIcon := AIcon;
  Add(AItem);
end;

end.


