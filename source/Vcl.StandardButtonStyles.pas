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
  Vcl.Graphics,
  Vcl.ButtonStylesAttributes;

const
  DEFAULT_CLASSIC_FAMILY = 'Classic';
  DEFAULT_WINDOWS_CLASS = 'Windows';
  DEFAULT_APPEARANCE = 'Normal';
  OUTLINE_APPEREANCE = 'Outline';
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
  end;

implementation

uses
  Winapi.Windows
  , Vcl.StyledButton
  , Vcl.StdCtrls
  , Vcl.Themes
  , System.SysUtils
  , System.UITypes
  , System.Math;

var
  ButtonClasses: TButtonClasses;
  ButtonAppearances: TButtonAppearances;

{ TStyledButtonStdStyle }

procedure StandardClassToColors(const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  var AFontColor, AButtonColor, ABorderColor: TColor; out AOutLine: Boolean);
var
  LStyle: TCustomStyleServices;
  Details:  TThemedElementDetails;
begin
  if SameText(AClass, DEFAULT_WINDOWS_CLASS) then
  begin
    AButtonColor := clBtnFace;
    ABorderColor := clBtnShadow;
    AFontColor := clWindowText;
    AOutLine := SameText(AAppearance, OUTLINE_APPEREANCE);
  end
  else
  begin
    LStyle := TStyleManager.Style[AClass];
    if (LStyle = nil) or not LStyle.Enabled then
      LStyle := StyleServices;
    Details := LStyle.GetElementDetails(tbPushButtonNormal);

    //TODO: mapping VCL Styles colors
    AButtonColor := LStyle.GetSystemColor(clBtnFace);
    ABorderColor := LStyle.GetSystemColor(clBtnShadow);
    LStyle.GetElementColor(Details, ecTextColor, AFontColor);
  end;
end;

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

function TButtonStandardStyles.GetButtonAppearances: TButtonAppearances;
begin
  if length(ButtonAppearances) = 0 then
  begin
    SetLength(ButtonAppearances, 2);
    ButtonAppearances[0] := DEFAULT_APPEARANCE;
    ButtonAppearances[1] := OUTLINE_APPEREANCE;
  end;
  Result := ButtonAppearances;
end;

procedure TButtonStandardStyles.UpdateAttributes(
  const AFamily:  TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  var ANormalStyle, APressedStyle, ASelectedStyle, AHotStyle,
  ADisabledStyle: TStyledButtonAttributes);
var
  LFontColor, LButtonColor, LBorderColor: TColor;
  LOutLine: Boolean;
begin
  StandardClassToColors(AClass, AAppearance, LFontColor, LButtonColor, LBorderColor, LOutLine);
  //Default Style Attributes
  ANormalStyle.BorderWidth := STD_BORDER_WIDTH;
  ANormalStyle.BorderColor := LBorderColor;
  ANormalStyle.BorderDrawStyle := brdSolid;
  ANormalStyle.FontColor := LFontColor;
  ANormalStyle.ButtonColor := LButtonColor;

  //Clone Normal Style to Other Styles
  CloneButtonStyle(ANormalStyle, APressedStyle);
  CloneButtonStyle(ANormalStyle, ASelectedStyle);
  CloneButtonStyle(ANormalStyle, AHotStyle);
  CloneButtonStyle(ANormalStyle, ADisabledStyle);

  if LOutline then
  begin
    //Button Down: color as Button Color
    with APressedStyle do
    begin
      ButtonColor := LButtonColor;
      BorderColor := LightenColor(LButtonColor, 50);
      BorderDrawStyle := brdSolid;
      BorderWidth := STD_BORDER_WIDTH;
      FontColor   := LFontColor;
      ButtonDrawStyle  := btnSolid;
    end;

    //Button Hot: color as Button Color
    with AHotStyle do
    begin
      ButtonColor := LButtonColor;
      BorderColor := LightenColor(LButtonColor, 50);
      BorderDrawStyle := brdClear;
      BorderWidth := STD_BORDER_WIDTH;
      FontColor := LFontColor;
      ButtonDrawStyle  := btnSolid;
    end;

    //Button Selected
    with ASelectedStyle do
    begin
      if Pos('dark', AClass) > 0 then
        ButtonColor := LightenColor(LButtonColor, 20)
      else
        ButtonColor := DarkenColor(LButtonColor, 20);
      BorderDrawStyle := brdSolid;
      BorderWidth := STD_BORDER_WIDTH;
      FontColor := LFontColor;
      ButtonDrawStyle  := btnSolid;
    end;

    //Button Disabled
    with ADisabledStyle do
    begin
      ButtonColor := LightenColor(ANormalStyle.ButtonColor, 70); //ColortoGrayscale(LButtonColor);
      FontColor := LightenColor(LFontColor, 70);
    end;
  end
  else
  begin
    //Button Down
    APressedStyle.ButtonColor := DarkenColor(LButtonColor, 20);
    APressedStyle.BorderColor := LightenColor(LBorderColor, 50);
    APressedStyle.BorderDrawStyle := brdSolid;
    APressedStyle.BorderWidth := STD_BORDER_WIDTH;

    //Button Hot: color as Down but no Border
    AHotStyle.ButtonColor := APressedStyle.ButtonColor;

    //Button Focused
    ASelectedStyle.ButtonColor := DarkenColor(LButtonColor, 20);
    ASelectedStyle.BorderDrawStyle := brdSolid;
    ASelectedStyle.BorderWidth := STD_BORDER_WIDTH;

    //Button Disabled
    ADisabledStyle.ButtonColor := LightenColor(ANormalStyle.ButtonColor, 70);//ColortoGrayscale(LButtonColor);
    ADisabledStyle.FontColor := LightenColor(LFontColor, 70);
  end;
end;

initialization
  SetLength(ButtonClasses,0);
  SetLength(ButtonAppearances,0);

  RegisterButtonFamily(
    TButtonStandardStyles.Create);

end.
