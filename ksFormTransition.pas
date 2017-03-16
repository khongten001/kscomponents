{*******************************************************************************
*                                                                              *
*  TksFormTransition - Push/Pop Form Queue Component                                *
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

unit ksFormTransition;

interface

{$I ksComponents.inc}

uses System.UITypes, FMX.Controls, FMX.Layouts, FMX.Objects, System.Classes,
  FMX.Types, Generics.Collections, FMX.Graphics, System.UIConsts, FMX.Effects,
  FMX.StdCtrls, System.Types, FMX.Forms, ksTypes, System.Generics.Collections;

const
  {$IFDEF ANDROID}
  C_TRANSITION_DURATION = 0.3;
  {$ELSE}
  C_TRANSITION_DURATION = 0.3;
  {$ENDIF}
  C_FADE_OPACITY = 0.5;

type
  TksTransitionType = (ksFtSlideInFromLeft,
                       ksFtSlideInFromTop,
                       ksFtSlideInFromRight,
                       ksFtSlideInFromBottom,
                       ksFtSlideOutToLeft,
                       ksFtSlideOutToTop,
                       ksFtSlideOutToRight,
                       ksFtSlideOutToBottom,
                       ksFtNoTransition);

  TksFormTransitionItem = class
    FFromForm: TCommonCustomForm;
    FToForm: TCommonCustomForm;
    FTransition: TksTransitionType;
  public
    property FromForm: TCommonCustomForm read FFromForm;
    property ToForm: TCommonCustomForm read FToForm;
    property Transition: TksTransitionType read FTransition;
  end;

  TksFormTransitionList = class(TObjectList<TksFormTransitionItem>)
  public

  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64
    {$ELSE} pidiOSDevice {$ENDIF} or pidiOSSimulator or pidAndroid)]

  TksFormTransition = class(TksComponent)
  private
    function GetTransitionList: TksFormTransitionList;
  public
    procedure Push(AForm: TCommonCustomForm; ATransition: TksTransitionType);
    procedure Pop;
    property TransitionList: TksFormTransitionList read GetTransitionList;
  end;

  //{$R *.dcr}

  procedure Register;


implementation

uses FMX.Ani, SysUtils, ksCommon, DateUtils, ksFormTransitionUI;

var
  _InternalTransitionList: TksFormTransitionList;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksFormTransition]);
end;

{ TksFormTransition }

function TksFormTransition.GetTransitionList: TksFormTransitionList;
begin
  Result := _InternalTransitionList;
end;

procedure TksFormTransition.Pop;
var
  AInfo: TksFormTransitionItem;
  AFrom, ATo: TCommonCustomForm;
  AAnimateForm: TfrmFormTransitionUI;
begin
  if _InternalTransitionList.Count = 0 then
    Exit;

  AAnimateForm := TfrmFormTransitionUI.Create(nil);
  try

    AInfo := _InternalTransitionList.Last;

    AFrom := AInfo.FToForm;
    ATo := AInfo.FFromForm;


    AAnimateForm.Initialise(AFrom, ATo);
    AAnimateForm.Visible := True;
    AAnimateForm.Animate(AInfo.FTransition, True);
    ATo.Visible := True;
    AAnimateForm.Visible := False;


    AFrom.Visible := False;
    ATo.Activate;

    {$IFDEF MSWINDOWS}
    ATo.SetBounds(AFrom.Left, AFrom.Top, AFrom.Width, AFrom.Height);
    {$ENDIF}

    _InternalTransitionList.Delete(_InternalTransitionList.Count-1);
  finally
    AAnimateForm.DisposeOf;
  end;
end;

procedure TksFormTransition.Push(AForm: TCommonCustomForm; ATransition: TksTransitionType);
var
  AInfo: TksFormTransitionItem;
  AFrom, ATo: TCommonCustomForm;
  AAnimateForm: TfrmFormTransitionUI;
begin
  AFrom := Screen.ActiveForm;
  ATo := AForm;

  AAnimateForm := TfrmFormTransitionUI.Create(nil);
  try
    AInfo := TksFormTransitionItem.Create;
    AInfo.FFromForm := AFrom;
    AInfo.FToForm := ATo;


    AInfo.FTransition := ATransition;
    _InternalTransitionList.Add(AInfo);

    AAnimateForm.Initialise(AFrom, ATo);
    AAnimateForm.Visible := True;
    AAnimateForm.Animate(AInfo.FTransition, False);
    AForm.Visible := True;
    AAnimateForm.Visible := False;
    AFrom.Visible := False;
    AForm.Activate;
    {$IFDEF MSWINDOWS}
    AForm.SetBounds(AFrom.Left, AFrom.Top, AFrom.Width, AFrom.Height);
    {$ENDIF}
  finally
    AAnimateForm.DisposeOf;
  end;
end;

initialization

  _InternalTransitionList := TksFormTransitionList.Create(True);

finalization

  FreeAndNil(_InternalTransitionList);

end.


