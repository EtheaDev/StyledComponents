{******************************************************************************}
{                                                                              }
{  TemplateButtonStyles: A base unit to create Button Styles attributes:       }
{  family/classes/appearance                                                   }
{  In this simple example a "semaphore" Family with four classes:              }
{  Green, Yellow, Red, Off in two appearance: Normal and Outline               }
{                                                                              }
{  Copyright (c) 2022-2024 (Ethea S.r.l.)                                      }
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
unit Vcl.ThemeAwareButtonStyles;

interface

uses
  Vcl.Graphics
  , System.UITypes
  , Vcl.ButtonStylesAttributes, Vcl.Themes;

const
  THEME_AWARE_STYLEDRAWTYPE = btRounded;

  //Name of Family
  THEME_AWARE_FAMILY = 'Theme Aware';

  //Name of Classes
  THEME_AWARE_CLASS = 'Theme Colors';

  //Name of Appearance
  THEME_AWARE_NORMAL_APPEARANCE = 'Normal';
  THEME_AWARE_OUTLINE_APPEARANCE = 'Outline';

  //Defaults
  THEME_AWARE_BORDER_WIDTH = 2;

Type
  TThemeAwareButtonStyles = class(TInterfacedObject, IStyledButtonAttributes)
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
function TThemeAwareButtonStyles.ButtonFamilyName: string;
begin
  Result := THEME_AWARE_FAMILY;
end;

// Do not change this method
function TThemeAwareButtonStyles.GetButtonAppearances: TButtonAppearances;
begin
  Result := ButtonAppearances;
end;

// Do not change this method
function TThemeAwareButtonStyles.GetButtonClasses: TButtonClasses;
begin
  Result := ButtonClasses;
end;

procedure TThemeAwareButtonStyles.GetStyleByModalResult(
  const AModalResult: System.UITypes.TModalResult;
  var AStyleClass: TStyledButtonClass;
  var AStyleAppearance: TStyledButtonAppearance);
begin
  //Define your button style based on ModalResult, used when you set ModalResult to your StyledButton
  //or when your family is used inside a TStyledTaskDialog
//  case AModalResult of
//    mrNone     : begin AStyleClass := BTN_OFF; AStyleAppearance := BTN_NORMAL; end;
//    mrYes      : begin AStyleClass := BTN_GREEN; AStyleAppearance := BTN_NORMAL; end;
//    mrNo       : begin AStyleClass := BTN_RED; AStyleAppearance := BTN_NORMAL; end;
//    mrOk       : begin AStyleClass := BTN_GREEN; AStyleAppearance := BTN_NORMAL; end;
//    mrCancel   : begin AStyleClass := BTN_RED; AStyleAppearance := BTN_NORMAL; end;
//    mrAbort    : begin AStyleClass := BTN_RED; AStyleAppearance := BTN_NORMAL; end;
//    mrRetry    : begin AStyleClass := BTN_YELLOW; AStyleAppearance := BTN_NORMAL; end;
//    mrIgnore   : begin AStyleClass := BTN_GREEN; AStyleAppearance := BTN_NORMAL; end;
//    mrAll      : begin AStyleClass := BTN_GREEN; AStyleAppearance := BTN_OUTLINE; end;
//    mrNoToAll  : begin AStyleClass := BTN_RED; AStyleAppearance := BTN_OUTLINE; end;
//    mrYesToAll : begin AStyleClass := BTN_GREEN; AStyleAppearance := BTN_OUTLINE; end;
//    mrClose    : begin AStyleClass := BTN_OFF; AStyleAppearance := BTN_NORMAL; end;
//    mrTryAgain : begin AStyleClass := BTN_GREEN; AStyleAppearance := BTN_NORMAL; end;
//    mrContinue : begin AStyleClass := BTN_YELLOW; AStyleAppearance := BTN_NORMAL; end;
//    mrHelp     : begin AStyleClass := BTN_OFF; AStyleAppearance := BTN_NORMAL; end;
//  end;

  AStyleClass := THEME_AWARE_CLASS;
  AStyleAppearance := THEME_AWARE_NORMAL_APPEARANCE;
end;

procedure TThemeAwareButtonStyles.UpdateAttributes(
  const AFamily:  TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  var ANormalStyle, APressedStyle, ASelectedStyle, AHotStyle,
  ADisabledStyle: TStyledButtonAttributes);
var
  LFontColor, LButtonColor: TColor;
  LOutLine: Boolean;
begin
  LButtonColor := TStyleManager.ActiveStyle.GetStyleColor( scButtonNormal );
  LFontColor := TStyleManager.ActiveStyle.GetStyleFontColor( sfButtonTextNormal );

  //Define outline attribute
  LOutLine := SameText(AAppearance, THEME_AWARE_OUTLINE_APPEARANCE);

  //Default Style Attributes for Semaphore Buttons
  ANormalStyle.DrawType := btRounded;
  ANormalStyle.BorderWidth := THEME_AWARE_BORDER_WIDTH;

  //Style for Normal Style of Semaphore Button
  if LOutLine then
  begin
    //Outline Appearance: Border and FontColor same as Button Color and Bold Font
    ANormalStyle.ButtonDrawStyle := btnClear;
    ANormalStyle.BorderDrawStyle := brdSolid;
    ANormalStyle.FontStyle := [fsBold];
    ANormalStyle.FontColor := LButtonColor;
    ANormalStyle.BorderColor := TStyleManager.ActiveStyle.GetStyleColor( scBorder );
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
      ButtonColor := TStyleManager.ActiveStyle.GetStyleColor( scButtonPressed );
      BorderColor := TStyleManager.ActiveStyle.GetStyleColor( scBorder );
      BorderDrawStyle := brdSolid;
      BorderWidth := THEME_AWARE_BORDER_WIDTH+2;
      FontColor   := TStyleManager.ActiveStyle.GetStyleFontColor( sfButtonTextPressed );;
    end;

    //Button Hot: apply a Darken color to Button Color
    with AHotStyle do
    begin
      ButtonDrawStyle  := btnSolid;
      ButtonColor := TStyleManager.ActiveStyle.GetStyleColor( scButtonHot );
      BorderDrawStyle := brdClear;
      BorderWidth := THEME_AWARE_BORDER_WIDTH;
      FontColor := TStyleManager.ActiveStyle.GetStyleFontColor( sfButtonTextHot );
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
      BorderWidth := THEME_AWARE_BORDER_WIDTH+2;
      BorderColor := TStyleManager.ActiveStyle.GetStyleColor( scBorder );
      ButtonDrawStyle  := btnClear;
      ButtonColor := TStyleManager.ActiveStyle.GetStyleColor( scButtonFocused );
      FontColor := TStyleManager.ActiveStyle.GetStyleFontColor( sfButtonTextFocused );
    end;

    //Button Disabled
    with ADisabledStyle do
    begin
      ButtonColor := TStyleManager.ActiveStyle.GetStyleColor( scButtonDisabled );
      FontColor :=TStyleManager.ActiveStyle.GetStyleFontColor( sfButtonTextDisabled );
    end;
  end
  else
  begin
    //Button Down
    APressedStyle.ButtonColor := TStyleManager.ActiveStyle.GetStyleColor( scButtonPressed );
    APressedStyle.FontColor := TStyleManager.ActiveStyle.GetStyleFontColor( sfButtonTextPressed );
    APressedStyle.BorderColor := TStyleManager.ActiveStyle.GetStyleColor( scBorder );
    APressedStyle.BorderDrawStyle := brdSolid;
    APressedStyle.BorderWidth := THEME_AWARE_BORDER_WIDTH;

    //Button Hot: color as Down but no Border
    AHotStyle.ButtonColor := TStyleManager.ActiveStyle.GetStyleColor( scButtonHot );
    AHotStyle.FontColor := TStyleManager.ActiveStyle.GetStyleFontColor( sfButtonTextHot );;

    //Button Focused
    ASelectedStyle.ButtonColor := TStyleManager.ActiveStyle.GetStyleColor( scButtonFocused );
    ASelectedStyle.FontColor := TStyleManager.ActiveStyle.GetStyleFontColor( sfButtonTextFocused );
    ASelectedStyle.BorderDrawStyle := brdSolid;
    ASelectedStyle.BorderWidth := THEME_AWARE_BORDER_WIDTH;

    //Button Disabled
    ADisabledStyle.ButtonColor := TStyleManager.ActiveStyle.GetStyleColor( scButtonDisabled );
    ADisabledStyle.FontColor := TStyleManager.ActiveStyle.GetStyleFontColor( sfButtonTextDisabled );
  end;
end;

initialization
  SetLength(ButtonClasses, 1);
  ButtonClasses[0] := THEME_AWARE_CLASS; //Default
//  ButtonClasses[1] := BTN_GREEN;
//  ButtonClasses[2] := BTN_YELLOW;
//  ButtonClasses[3] := BTN_RED;

  SetLength(ButtonAppearances, 2);
  ButtonAppearances[0] := THEME_AWARE_NORMAL_APPEARANCE; //Default
  ButtonAppearances[1] := THEME_AWARE_OUTLINE_APPEARANCE;

  RegisterButtonFamily(TThemeAwareButtonStyles.Create);

end.
