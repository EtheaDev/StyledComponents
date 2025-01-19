{******************************************************************************}
{                                                                              }
{  TemplateButtonStyles: A base unit to create Button Styles attributes:       }
{  family/classes/appearance                                                   }
{  In this simple example a "semaphore" Family with four classes:              }
{  Green, Yellow, Red, Off in two appearance: Normal and Outline               }
{                                                                              }
{  Copyright (c) 2022-2025 (Ethea S.r.l.)                                      }
{  Author: Carlo Barazzetta                                                    }
{  Contributors:                                                               }
{                                                                              }
{  https://github.com/EtheaDev/StyledComponents                                }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit Vcl.TemplateButtonStyles;

interface

uses
  Vcl.Graphics
  , System.UITypes
  , Vcl.ButtonStylesAttributes;

const
  //Name of Family
  BTN_FAMILY = 'Semaphore';

  //Name of Classes
  BTN_OFF = 'SemOff';
  BTN_GREEN = 'SemGreen';
  BTN_YELLOW = 'SemYellow';
  BTN_RED = 'SemRed';

  //Name of Appearance
  BTN_NORMAL = 'Normal';
  BTN_OUTLINE = 'Outline';

  //Defaults
  BTN_BORDER_WIDTH = 2;

Type
  TSemaphoreButtonStyles = class(TInterfacedObject, IStyledButtonAttributes)
  private
    procedure UpdateAttributes(
      const AFamily:  TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance;
      var ANormalStyle, APressedStyle, ASelectedStyle,
      AHotStyle, ADisabledStyle: TStyledButtonAttributes);
    function ButtonFamilyName: string;
    function GetButtonClasses: TButtonClasses;
    function GetButtonAppearances: TButtonAppearances;
    procedure GetStyleByModalResult(
      const AModalResult: System.UITypes.TModalResult;
      var AStyleClass: TStyledButtonClass;
      var AStyleAppearance: TStyledButtonAppearance);
  end;

implementation

uses
  System.SysUtils
  , WinApi.Windows;

var
  ButtonClasses: TButtonClasses;
  ButtonAppearances: TButtonAppearances;

{ TSemaphoreButtonStyles }

// Do not change this method
function TSemaphoreButtonStyles.ButtonFamilyName: string;
begin
  Result := BTN_FAMILY;
end;

// Do not change this method
function TSemaphoreButtonStyles.GetButtonAppearances: TButtonAppearances;
begin
  Result := ButtonAppearances;
end;

// Do not change this method
function TSemaphoreButtonStyles.GetButtonClasses: TButtonClasses;
begin
  Result := ButtonClasses;
end;

procedure TSemaphoreButtonStyles.GetStyleByModalResult(
  const AModalResult: System.UITypes.TModalResult;
  var AStyleClass: TStyledButtonClass;
  var AStyleAppearance: TStyledButtonAppearance);
begin
  //Define your button style based on ModalResult, used when you set ModalResult to your StyledButton
  //or when your family is used inside a TStyledTaskDialog
  case AModalResult of
    mrNone     : begin AStyleClass := BTN_OFF; AStyleAppearance := BTN_NORMAL; end;
    mrYes      : begin AStyleClass := BTN_GREEN; AStyleAppearance := BTN_NORMAL; end;
    mrNo       : begin AStyleClass := BTN_RED; AStyleAppearance := BTN_NORMAL; end;
    mrOk       : begin AStyleClass := BTN_GREEN; AStyleAppearance := BTN_NORMAL; end;
    mrCancel   : begin AStyleClass := BTN_RED; AStyleAppearance := BTN_NORMAL; end;
    mrAbort    : begin AStyleClass := BTN_RED; AStyleAppearance := BTN_NORMAL; end;
    mrRetry    : begin AStyleClass := BTN_YELLOW; AStyleAppearance := BTN_NORMAL; end;
    mrIgnore   : begin AStyleClass := BTN_GREEN; AStyleAppearance := BTN_NORMAL; end;
    mrAll      : begin AStyleClass := BTN_GREEN; AStyleAppearance := BTN_OUTLINE; end;
    mrNoToAll  : begin AStyleClass := BTN_RED; AStyleAppearance := BTN_OUTLINE; end;
    mrYesToAll : begin AStyleClass := BTN_GREEN; AStyleAppearance := BTN_OUTLINE; end;
    mrClose    : begin AStyleClass := BTN_OFF; AStyleAppearance := BTN_NORMAL; end;
    mrTryAgain : begin AStyleClass := BTN_GREEN; AStyleAppearance := BTN_NORMAL; end;
    mrContinue : begin AStyleClass := BTN_YELLOW; AStyleAppearance := BTN_NORMAL; end;
    mrHelp     : begin AStyleClass := BTN_OFF; AStyleAppearance := BTN_NORMAL; end;
  else
    GetStyleByModalResult(mrNone, AStyleClass, AStyleAppearance);
  end;
end;

procedure TSemaphoreButtonStyles.UpdateAttributes(
  const AFamily:  TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  var ANormalStyle, APressedStyle, ASelectedStyle, AHotStyle,
  ADisabledStyle: TStyledButtonAttributes);
var
  LFontColor, LButtonColor: TColor;
  LOutLine: Boolean;
begin
  //Define AButtonColor and AFontColor for standard State button
  if SameText(AClass, BTN_GREEN) then
  begin
    LButtonColor := clGreen;
    LFontColor := clWhite;
  end
  else if SameText(AClass, BTN_YELLOW) then
  begin
    LButtonColor := clYellow;
    LFontColor := clBlack;
  end
  else if SameText(AClass, BTN_RED) then
  begin
    LButtonColor := clRed;
    LFontColor := clWhite;
  end
  else// BTN_OFF (default)
  begin
    LButtonColor := clSilver;
    LFontColor := clGray;
  end;
  //Define outline attribute
  LOutLine := SameText(AAppearance, BTN_OUTLINE);

  //Default Style Attributes for Semaphore Buttons
  ANormalStyle.DrawType := btRounded;
  ANormalStyle.BorderWidth := BTN_BORDER_WIDTH;

  //Style for Normal Style of Semaphore Button
  if LOutLine then
  begin
    //Outline Appearance: Border and FontColor same as Button Color and Bold Font
    ANormalStyle.ButtonDrawStyle := btnClear;
    ANormalStyle.BorderDrawStyle := brdSolid;
  ANormalStyle.FontStyle := [fsBold];
    ANormalStyle.FontColor := LButtonColor;
    ANormalStyle.BorderColor := LightenColor(LButtonColor, 50);
  end
  else
  begin
    //Normal Appearance: Border and FontColor same as Button Color and Normal Font
    ANormalStyle.ButtonDrawStyle := btnSolid;
    ANormalStyle.BorderDrawStyle := brdClear;
    ANormalStyle.FontStyle := [];
    ANormalStyle.FontColor := LFontColor;
    ANormalStyle.ButtonColor := LButtonColor;
    ANormalStyle.BorderColor := LButtonColor;
  end;

  //Clone all attributes of Normal "State" to Other "States"
  CloneButtonStyle(ANormalStyle, APressedStyle);
  CloneButtonStyle(ANormalStyle, ASelectedStyle);
  CloneButtonStyle(ANormalStyle, AHotStyle);
  CloneButtonStyle(ANormalStyle, ADisabledStyle);

  if LOutline then
  begin
    //Button Down: color as Button Color and more Border
    with APressedStyle do
    begin
      ButtonDrawStyle  := btnSolid;
      ButtonColor := LButtonColor;
      BorderColor := LightenColor(LButtonColor, 50);
      BorderDrawStyle := brdSolid;
      BorderWidth := BTN_BORDER_WIDTH+2;
      FontColor   := LFontColor;
    end;

    //Button Hot: apply a Darken color to Button Color
    with AHotStyle do
    begin
      ButtonDrawStyle  := btnSolid;
      ButtonColor := DarkenColor(LButtonColor, 50);
      BorderDrawStyle := brdClear;
      BorderWidth := BTN_BORDER_WIDTH;
      FontColor := LFontColor;
    end;

    //Button Focused
    with ASelectedStyle do
    begin
(*
      if SameText(AClass, btn_dark) then
        ButtonColor := LightenColor(LButtonColor, 20)
      else
        ButtonColor := DarkenColor(LButtonColor, 20);
*)
      BorderDrawStyle := brdSolid;
      BorderWidth := BTN_BORDER_WIDTH+2;
      BorderColor := LightenColor(LButtonColor, 50);
      ButtonDrawStyle  := btnClear;
    end;

    //Button Disabled
    with ADisabledStyle do
    begin
      ButtonColor := LightenColor(ANormalStyle.ButtonColor, 70);
      FontColor := LightenColor(LFontColor, 70);
    end;
  end
  else
  begin
    //Button Down
    APressedStyle.ButtonColor := DarkenColor(LButtonColor, 20);
    APressedStyle.BorderColor := LightenColor(LButtonColor, 50);
    APressedStyle.BorderDrawStyle := brdSolid;
    APressedStyle.BorderWidth := BTN_BORDER_WIDTH;

    //Button Hot: color as Down but no Border
    AHotStyle.ButtonColor := APressedStyle.ButtonColor;

    //Button Focused
    ASelectedStyle.ButtonColor := DarkenColor(LButtonColor, 20);
    ASelectedStyle.BorderDrawStyle := brdSolid;
    ASelectedStyle.BorderWidth := BTN_BORDER_WIDTH;

    //Button Disabled
    ADisabledStyle.ButtonColor := LightenColor(ANormalStyle.ButtonColor, 70);//ColortoGrayscale(LButtonColor);
    ADisabledStyle.FontColor := LightenColor(LFontColor, 70);
  end;
end;

initialization
  SetLength(ButtonClasses, 4);
  ButtonClasses[0] := BTN_OFF; //Default
  ButtonClasses[1] := BTN_GREEN;
  ButtonClasses[2] := BTN_YELLOW;
  ButtonClasses[3] := BTN_RED;

  SetLength(ButtonAppearances, 2);
  ButtonAppearances[0] := BTN_NORMAL; //Default
  ButtonAppearances[1] := BTN_OUTLINE;

  RegisterButtonFamily(TSemaphoreButtonStyles.Create);

end.
