{*******************************************************************************
*                                                                              *
*  TksListViewFilter - Search Filter for TksVirtualListView                    *
*                                                                              *
*  https://bitbucket.org/gmurt/kscomponents                                  *
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

unit ksListViewFilter;

interface

{$I ksComponents.inc}


uses Classes, FMX.StdCtrls, FMX.Graphics, ksTypes, FMX.Objects,
  System.UITypes, System.UIConsts, FMX.Edit, FMX.Types;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64
    {$ELSE} pidiOSDevice {$ENDIF} or pidiOSSimulator or pidAndroid)]

  TksListViewFilter = class(TCustomEdit)
  private
    FTimer: TTimer;
    FClearButton: TButton;
    procedure DoTimer(Sender: TObject);
    procedure DoClickClear(Sender: TObject);
    procedure ClearButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  protected
    procedure SetText(const Value: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Unfocus;
  published
  end;

  procedure Register;

implementation



uses Math, System.TypInfo, System.Types, ksCommon, SysUtils,
  Fmx.Forms, FMX.Controls;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksListViewFilter]);
end;

{ TksSpeedButton }

procedure TksListViewFilter.ClearButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  Text := '';
  FClearButton.Visible := False;
end;

constructor TksListViewFilter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if (csDesigning in ComponentState) then exit;

  FTimer := TTimer.Create(nil);
  FTimer.Interval := 1000;
  FTimer.OnTimer := DoTimer;
  FClearButton := TButton.Create(Self);
  FClearButton.Name := Name + 'FilterButton';
  FClearButton.Align := TAlignLayout.Right;
  FClearButton.TextSettings.Font.Size := 13;
  FClearButton.StyledSettings := [TStyledSetting.Family,TStyledSetting.Style,TStyledSetting.FontColor];
  FClearButton.Text := 'CLEAR';
  FClearButton.HitTest := True;
  FClearButton.CanFocus := False;
  StyleLookup := 'searcheditbox';
  FClearButton.OnClick := DoClickClear;
  FClearButton.OnMouseDown := ClearButtonMouseDown;
  FClearButton.Visible := False;
  AddObject(FClearButton);
end;

destructor TksListViewFilter.Destroy;
begin
  FClearButton.DisposeOf;
  FreeAndNil(FTimer);
  inherited;
end;


procedure TksListViewFilter.DoClickClear(Sender: TObject);
begin
  Text := '';
  FClearButton.Visible := False;
end;

procedure TksListViewFilter.DoTimer(Sender: TObject);
begin
  FClearButton.Visible := Text <> '';
end;

procedure TksListViewFilter.SetText(const Value: string);
begin
  inherited;

end;

procedure TksListViewFilter.Unfocus;
begin
  Root.SetFocused(nil);
end;

end.
