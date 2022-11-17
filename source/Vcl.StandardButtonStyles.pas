{******************************************************************************}
{                                                                              }
{       Standard Button Family: an example of "standard" Family                }
{       attributs for StyledButton                                             }
{                                                                              }
{       Copyright (c) 2022 (Ethea S.r.l.)                                      }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
{                                                                              }
{       https://github.com/EtheaDev/StyledComponents                           }
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
unit Vcl.StandardButtonStyles;

interface

uses
  Vcl.Graphics
  , System.UITypes
  , Vcl.ButtonStylesAttributes;

const
  DEFAULT_CLASSIC_FAMILY = 'Classic';
  DEFAULT_WINDOWS_CLASS = 'Windows';
  DEFAULT_APPEARANCE = 'Normal';
  OUTLINE_APPEARANCE = 'Outline';
  STD_BORDER_WIDTH = 2;

Type
  TButtonStandardStyles = class(TInterfacedObject, IStyledButtonAttributes)
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
  Winapi.Windows
  , Vcl.StyledButton
  , Vcl.StdCtrls
  , Vcl.Themes
  , System.SysUtils
  , System.Math;

var
  ButtonClasses: TButtonClasses;
  ButtonAppearances: TButtonAppearances;

{ TStyledButtonStdStyle }

function TButtonStandardStyles.ButtonFamilyName: string;
begin
  Result := DEFAULT_CLASSIC_FAMILY;
end;

function TButtonStandardStyles.GetButtonClasses: TButtonClasses;
var
  LStylesCount: Integer;
  LStyleName: string;
  I: Integer;
begin
  if length(ButtonClasses) = 0 then
  begin
    LStylesCount := Length(TStyleManager.StyleNames);
    SetLength(ButtonClasses,LStylesCount);
    for I := 0 to High(TStyleManager.StyleNames) do
    begin
      LStyleName := TStyleManager.StyleNames[i];
      ButtonClasses[I] := LStyleName;
    end;
  end;
  Result := ButtonClasses;
end;

procedure TButtonStandardStyles.GetStyleByModalResult(
  const AModalResult: System.UITypes.TModalResult;
  var AStyleClass: TStyledButtonClass;
  var AStyleAppearance: TStyledButtonAppearance);
begin
  //if AModalResult is mrNone, define the defaults of Family
  case AModalResult of
    mrNone     : begin AStyleClass := DEFAULT_WINDOWS_CLASS; AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrYes      : begin AStyleClass := DEFAULT_WINDOWS_CLASS; AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrNo       : begin AStyleClass := DEFAULT_WINDOWS_CLASS; AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrOk       : begin AStyleClass := DEFAULT_WINDOWS_CLASS; AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrCancel   : begin AStyleClass := DEFAULT_WINDOWS_CLASS; AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrAbort    : begin AStyleClass := DEFAULT_WINDOWS_CLASS; AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrRetry    : begin AStyleClass := DEFAULT_WINDOWS_CLASS; AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrIgnore   : begin AStyleClass := DEFAULT_WINDOWS_CLASS; AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrAll      : begin AStyleClass := DEFAULT_WINDOWS_CLASS; AStyleAppearance := OUTLINE_APPEARANCE; end;
    mrNoToAll  : begin AStyleClass := DEFAULT_WINDOWS_CLASS; AStyleAppearance := OUTLINE_APPEARANCE; end;
    mrYesToAll : begin AStyleClass := DEFAULT_WINDOWS_CLASS; AStyleAppearance := OUTLINE_APPEARANCE; end;
    mrClose    : begin AStyleClass := DEFAULT_WINDOWS_CLASS; AStyleAppearance := OUTLINE_APPEARANCE; end;
    mrTryAgain : begin AStyleClass := DEFAULT_WINDOWS_CLASS; AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrContinue : begin AStyleClass := DEFAULT_WINDOWS_CLASS; AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrHelp     : begin AStyleClass := DEFAULT_WINDOWS_CLASS; AStyleAppearance := OUTLINE_APPEARANCE; end;
  end;
end;

function TButtonStandardStyles.GetButtonAppearances: TButtonAppearances;
begin
  if length(ButtonAppearances) = 0 then
  begin
    SetLength(ButtonAppearances, 2);
    ButtonAppearances[0] := DEFAULT_APPEARANCE;
    ButtonAppearances[1] := OUTLINE_APPEARANCE;
  end;
  Result := ButtonAppearances;
end;

procedure StandardClassToColors(const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  var AFontColor, AButtonColor, ABorderColor: TColor; out AOutLine, ADarkStyle: Boolean);
var
  LStyle: TCustomStyleServices;
  Details:  TThemedElementDetails;
begin
  if SameText(AClass, DEFAULT_WINDOWS_CLASS) then
  begin
    if StyleServices.Enabled and not StyleServices.IsSystemStyle then
    begin
      //TODO: mapping VCL Button Styles colors and Dark/Light Style
      LStyle := StyleServices;
      AButtonColor := LStyle.GetSystemColor(clBtnFace);
      ABorderColor := LStyle.GetSystemColor(clBtnShadow);
      Details := LStyle.GetElementDetails(tbPushButtonNormal);
      LStyle.GetElementColor(Details, ecTextColor, AFontColor);
      ADarkStyle := (Pos('Dark', LStyle.Name) > 0) or (Pos('Gray', LStyle.Name) > 0);
    end
    else
    begin
      ADarkStyle := False;
      AButtonColor := clBtnFace;
      ABorderColor := clBtnShadow;
      AFontColor := clWindowText;
    end;
  end
  else
  begin
    LStyle := TStyleManager.Style[AClass];
    if (LStyle = nil) or not LStyle.Enabled then
      LStyle := StyleServices;
    Details := LStyle.GetElementDetails(tbPushButtonNormal);

    //TODO: mapping VCL Button Styles colors and Dark/Light Style
    AButtonColor := LStyle.GetSystemColor(clBtnFace);
    ABorderColor := LStyle.GetSystemColor(clBtnShadow);
    LStyle.GetElementColor(Details, ecTextColor, AFontColor);
    ADarkStyle := (Pos('Dark', LStyle.Name) > 0) or (Pos('Gray', LStyle.Name) > 0);
  end;
  AOutLine := SameText(AAppearance, OUTLINE_APPEARANCE);
end;

procedure TButtonStandardStyles.UpdateAttributes(
  const AFamily:  TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  var ANormalStyle, APressedStyle, ASelectedStyle, AHotStyle,
  ADisabledStyle: TStyledButtonAttributes);
var
  LDummyColor, LFontColor, LButtonColor, LBorderColor: TColor;
  LOutLine: Boolean;
  LDarkStyle: Boolean;
begin
  StandardClassToColors(AClass, AAppearance, LFontColor, LButtonColor, LBorderColor, LOutLine, LDarkStyle);
  //Outline: Invert Border Color and Button Color
  if LOutline then
  begin
    LDummyColor := LBorderColor;
    LBorderColor := LButtonColor;
    LButtonColor := LDummyColor;
  end;

  //Default Style Attributes for Standard Buttons
  ANormalStyle.DrawType := btRounded;
  ANormalStyle.FontStyle := [fsBold];
  ANormalStyle.FontColor := LFontColor;
  ANormalStyle.ButtonDrawStyle := btnSolid;
  ANormalStyle.ButtonColor := LButtonColor;
  ANormalStyle.BorderDrawStyle := brdSolid;
  ANormalStyle.BorderWidth := STD_BORDER_WIDTH;
  ANormalStyle.BorderColor := LBorderColor;

  //Clone Normal Style to Other Styles
  CloneButtonStyle(ANormalStyle, APressedStyle);
  CloneButtonStyle(ANormalStyle, ASelectedStyle);
  CloneButtonStyle(ANormalStyle, AHotStyle);
  CloneButtonStyle(ANormalStyle, ADisabledStyle);

  //Pressed Button
  if LDarkStyle then
  begin
    APressedStyle.ButtonColor := LightenColor(LButtonColor, 20);
    APressedStyle.BorderColor := DarkenColor(LBorderColor, 50);
  end
  else
  begin
    APressedStyle.ButtonColor := DarkenColor(LButtonColor, 20);
    APressedStyle.BorderColor := LightenColor(LBorderColor, 50);
  end;

  //Selected Button (focused)
  if LDarkStyle then
    ASelectedStyle.ButtonColor := LightenColor(LButtonColor, 20)
  else
    ASelectedStyle.ButtonColor := DarkenColor(LButtonColor, 20);
  ASelectedStyle.BorderDrawStyle := brdSolid;
  ASelectedStyle.BorderWidth := STD_BORDER_WIDTH;

  //Hot Button
  if LDarkStyle then
    AHotStyle.ButtonColor := LightenColor(ASelectedStyle.ButtonColor, 50)
  else
    AHotStyle.ButtonColor := DarkenColor(ASelectedStyle.ButtonColor, 50);

  //Button Disabled
  if LDarkStyle then
  begin
    ADisabledStyle.ButtonColor := DarkenColor(ANormalStyle.ButtonColor, 70);
    ADisabledStyle.FontColor := DarkenColor(LFontColor, 70);
  end
  else
  begin
    ADisabledStyle.ButtonColor := LightenColor(ANormalStyle.ButtonColor, 70);
    ADisabledStyle.FontColor := LightenColor(LFontColor, 70);
  end;
end;

initialization
  SetLength(ButtonClasses,0);
  SetLength(ButtonAppearances,0);

  RegisterButtonFamily(
    TButtonStandardStyles.Create);

end.
