{******************************************************************************}
{                                                                              }
{       AngulaButtonStyles: Button Styles inspired to Material/angular         }
{       https://material.angular.io/components/button/overview                 }
{                                                                              }
{       Copyright (c) 2022-2023 (Ethea S.r.l.)                                 }
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
unit Vcl.AngularButtonStyles;

interface

uses
  Vcl.Graphics
  , System.UITypes
  , Vcl.ButtonStylesAttributes;

const
  ANGULAR_STROKED_WIDTH = 2;
  ANGULAR_RAISED_WIDTH = 3;

  //Button Family as Angular Themes
  ANGULAR_LIGHT_FAMILY = 'Angular-Light';
  ANGULAR_DARK_FAMILY = 'Angular-Dark';

  //Button Class as Angular Styling
  btn_Basic = 'Basic';
  btn_Warn = 'Warn';
  btn_Link = 'Link';

  //DeepPurple & Amber Light Theme
  btn_PrimaryDeepPurple = 'DeepPurple';
  btn_AccentAmber = 'Amber';
  //Indigo & Pink Light Theme
  btn_PrimaryIndigo = 'Indigo';
  btn_AccentPink = 'Pink';

  //Pink & BlueGray Dark Theme
  btn_PrimaryPink = 'Pink';
  btn_AccentBlueGray = 'Blue-gray';
  //Purple & Green Dark Theme
  btn_PrimaryPurple = 'Purple';
  btn_AccentGreen = 'Green';

  //Button Appearance as Angular Attributes
  FlatAttr = 'Flat';
  RaisedAttr = 'Raised';
  BasicAttr = 'Basic';
  StrokedAttr = 'Stroked';

Type
  //Base class for light styles
  TAngularButtonStyleLight = class(TInterfacedObject, IStyledButtonAttributes)
  private
    procedure AngularClassToLightColors(const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance; var AFontColor,
      AButtonColor: TColor);
    //Implementation of IStyledButtonAttributes interface
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

  //Base class for dark styles
  TAngularButtonStyleDark = class(TInterfacedObject, IStyledButtonAttributes)
  private
    procedure AngularClassToDarkColors(const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance; var AFontColor,
      AButtonColor: TColor);
    //Implementation of IStyledButtonAttributes interface
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
  LightButtonClasses: TButtonClasses;
  DarkButtonClasses: TButtonClasses;
  ButtonAppearances: TButtonAppearances;

{ TAngularButtonStyleLight }

procedure TAngularButtonStyleLight.AngularClassToLightColors(const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  var AFontColor, AButtonColor: TColor);
begin
  //Button and Font Colors for Light Themes (DeepPurpleAndAmber)
  if SameText(AClass, btn_Basic) then
  begin
    AButtonColor := htmlToColor('#FFFFFF');
    AFontColor := htmlToColor('#212121');
  end
  else if SameText(AClass, btn_PrimaryDeepPurple) then
  begin
    AButtonColor := htmlToColor('#673AB7');
    AFontColor := htmlToColor('#FFFFFF');
  end
  else if SameText(AClass, btn_AccentAmber) then
  begin
    AButtonColor := htmlToColor('#FFD740');
    AFontColor := htmlToColor('#211C08');
  end
  else if SameText(AClass, btn_PrimaryIndigo) then
  begin
    AButtonColor := htmlToColor('#3F51B5');
    AFontColor := htmlToColor('#FFFFFF');
  end
  else if SameText(AClass, btn_AccentPink) then
  begin
    AButtonColor := htmlToColor('#FF4081');
    AFontColor := htmlToColor('#FFFFFF');
  end
  else if SameText(AClass, btn_Warn) then
  begin
    AButtonColor := htmlToColor('#F44336');
    AFontColor := htmlToColor('#FFFFFF');
  end
  else if SameText(AClass, btn_Link) then
  begin
    AButtonColor := htmlToColor('#FFFFFF');
    AFontColor := htmlToColor('#212121');
  end;
end;

function TAngularButtonStyleLight.ButtonFamilyName: string;
begin
  Result := ANGULAR_LIGHT_FAMILY;
end;

function TAngularButtonStyleLight.GetButtonAppearances: TButtonAppearances;
begin
  Result := ButtonAppearances;
end;

function TAngularButtonStyleLight.GetButtonClasses: TButtonClasses;
begin
  Result := LightButtonClasses;
end;

procedure TAngularButtonStyleLight.GetStyleByModalResult(
  const AModalResult: System.UITypes.TModalResult;
  var AStyleClass: TStyledButtonClass;
  var AStyleAppearance: TStyledButtonAppearance);
begin
  //if AModalResult is mrNone, define the defaults of Family
  case AModalResult of
    mrNone     : begin AStyleClass := btn_Basic; AStyleAppearance := FlatAttr; end;
    mrYes      : begin AStyleClass := btn_PrimaryDeepPurple; AStyleAppearance := FlatAttr; end;
    mrNo       : begin AStyleClass := btn_Warn; AStyleAppearance := FlatAttr; end;
    mrOk       : begin AStyleClass := btn_PrimaryDeepPurple; AStyleAppearance := FlatAttr; end;
    mrCancel   : begin AStyleClass := btn_Warn; AStyleAppearance := FlatAttr; end;
    mrAbort    : begin AStyleClass := btn_Warn; AStyleAppearance := RaisedAttr; end;
    mrRetry    : begin AStyleClass := btn_PrimaryDeepPurple; AStyleAppearance := RaisedAttr; end;
    mrIgnore   : begin AStyleClass := btn_PrimaryDeepPurple; AStyleAppearance := RaisedAttr; end;
    mrAll      : begin AStyleClass := btn_PrimaryDeepPurple; AStyleAppearance := RaisedAttr; end;
    mrNoToAll  : begin AStyleClass := btn_Warn; AStyleAppearance := RaisedAttr; end;
    mrYesToAll : begin AStyleClass := btn_PrimaryDeepPurple; AStyleAppearance := RaisedAttr; end;
    mrClose    : begin AStyleClass := btn_Warn; AStyleAppearance := FlatAttr; end;
    mrTryAgain : begin AStyleClass := btn_PrimaryDeepPurple; AStyleAppearance := FlatAttr; end;
    mrContinue : begin AStyleClass := btn_PrimaryDeepPurple; AStyleAppearance := FlatAttr; end;
    mrHelp     : begin AStyleClass := btn_Warn; AStyleAppearance := FlatAttr; end;
  end;
end;

procedure TAngularButtonStyleLight.UpdateAttributes(
  const AFamily:  TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  var ANormalStyle, APressedStyle, ASelectedStyle, AHotStyle,
  ADisabledStyle: TStyledButtonAttributes);
var
  LFontColor, LButtonColor: TColor;
  LRaised, LStroked, LBasic: Boolean;
  LPrimaryAccentWarn: Boolean;
begin
  AngularClassToLightColors(AClass, AAppearance, LFontColor, LButtonColor);

  //Default Style Attributes for Angular Buttons: Flat
  //using Flat Style as base style
  ANormalStyle.DrawType := btRounded;
  ANormalStyle.BorderWidth := 0;
  ANormalStyle.BorderDrawStyle := brdClear;
  ANormalStyle.FontStyle := [fsBold];
  ANormalStyle.ButtonColor := LButtonColor;
  ANormalStyle.FontColor := LFontColor;

  LStroked := SameText(AAppearance, StrokedAttr);
  LRaised := SameText(AAppearance, RaisedAttr);
  LPrimaryAccentWarn :=
    SameText(AClass, btn_PrimaryDeepPurple) or
    SameText(AClass, btn_PrimaryIndigo) or
    SameText(AClass, btn_AccentAmber) or
    SameText(AClass, btn_AccentPink) or
    SameText(AClass, btn_Warn);
  LBasic := SameText(AAppearance, BasicAttr);

  if LStroked then
  begin
    //Every stroked Buttons has thin border and
    //and no color for the Button
    ANormalStyle.BorderWidth := ANGULAR_STROKED_WIDTH;
    ANormalStyle.BorderDrawStyle := brdSolid;
    ANormalStyle.ButtonDrawStyle := btnClear;
    //Only for Primary, Accent and Warn, Stroked FontColor as Button Color
    if LPrimaryAccentWarn then
      ANormalStyle.FontColor := LButtonColor
    else
      ANormalStyle.FontColor := LFontColor;
  end
  else if LRaised then
  begin
    //Raised Button can be showed with a darker border color
    //from button color, because we don't have "shadow" effect
    ANormalStyle.BorderWidth := ANGULAR_RAISED_WIDTH;
    ANormalStyle.BorderDrawStyle := brdSolid;
    ANormalStyle.BorderColor := DarkenColor(ANormalStyle.ButtonColor, 20);
  end
  else if LBasic then
  begin
    //Basic Buttons has FontColor as Flat button Color
    //But no Color for the Button
    ANormalStyle.ButtonDrawStyle := btnClear;
    //Only for Primary, Accent and Warn, Stroked FontColor as Button Color
    if LPrimaryAccentWarn then
      ANormalStyle.FontColor := LButtonColor
    else
      ANormalStyle.FontColor := LFontColor;
  end;

  //Clone Normal Style to Other Styles
  CloneButtonStyle(ANormalStyle, APressedStyle);
  CloneButtonStyle(ANormalStyle, ASelectedStyle);
  CloneButtonStyle(ANormalStyle, AHotStyle);
  CloneButtonStyle(ANormalStyle, ADisabledStyle);

  if LStroked or LBasic then
  begin
    //Button Hot: Button Color 50% ligthen of Font Color
    AHotStyle.ButtonDrawStyle := btnSolid;
    AHotStyle.ButtonColor := LightenColor(AHotStyle.FontColor, 50);

    //Button Pressed: Button Color 40% ligthen of Font Color
    APressedStyle.ButtonDrawStyle := btnSolid;
    APressedStyle.ButtonColor := LightenColor(ASelectedStyle.FontColor, 40);

    //Button Selected: Button Color 60% ligthen of Font Color
    ASelectedStyle.ButtonDrawStyle := btnSolid;
    ASelectedStyle.ButtonColor := LightenColor(ASelectedStyle.FontColor, 60);

    //Button Disabled
    ADisabledStyle.FontColor := htmlToColor('#A3A3A3');
  end
  else
  begin
    //Flat and Raised Appearance
    AHotStyle.ButtonColor := DarkenColor(AHotStyle.ButtonColor, 10);

    //Button Focused: botton color Darken
    ASelectedStyle.ButtonColor :=  DarkenColor(ASelectedStyle.ButtonColor, 10);

    //Button Down: botton color Darken then Focused
    APressedStyle.ButtonColor :=  DarkenColor(APressedStyle.ButtonColor, 20);

    //Button Disabled: Gray Button and Font
    ADisabledStyle.BorderWidth := 0;
    ADisabledStyle.ButtonColor := htmlToColor('#DCDCDC');
    ADisabledStyle.FontColor := htmlToColor('#A3A3A3');
  end;
end;

{ TAngularButtonStyleDark }

procedure TAngularButtonStyleDark.AngularClassToDarkColors(const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  var AFontColor, AButtonColor: TColor);
begin
  //Button and Font Colors for Light Themes (DeepPurpleAndAmber)
  if SameText(AClass, btn_Basic) then
  begin
    AButtonColor := htmlToColor('#424242');
    AFontColor := htmlToColor('#FFFFFF');
  end
  else if SameText(AClass, btn_PrimaryPink) then
  begin
    AButtonColor := htmlToColor('#E91E63');
    AFontColor := htmlToColor('#FFFFFF');
  end
  else if SameText(AClass, btn_AccentBlueGray) then
  begin
    AButtonColor := htmlToColor('#607D8B');
    AFontColor := htmlToColor('#FFFFFF');
  end
  else if SameText(AClass, btn_PrimaryPurple) then
  begin
    AButtonColor := htmlToColor('#9C27B0');
    AFontColor := htmlToColor('#FFFFFF');
  end
  else if SameText(AClass, btn_AccentGreen) then
  begin
    AButtonColor := htmlToColor('#69F0AE');
    AFontColor := htmlToColor('#0E1F17');
  end
  else if SameText(AClass, btn_Warn) then
  begin
    AButtonColor := htmlToColor('#F44336');
    AFontColor := htmlToColor('#FFFFFF');
  end
  else if SameText(AClass, btn_Link) then
  begin
    AButtonColor := htmlToColor('#424242');
    AFontColor := htmlToColor('#FFFFFF');
  end;
end;

function TAngularButtonStyleDark.ButtonFamilyName: string;
begin
  Result := ANGULAR_DARK_FAMILY;
end;

function TAngularButtonStyleDark.GetButtonAppearances: TButtonAppearances;
begin
  Result := ButtonAppearances;
end;

function TAngularButtonStyleDark.GetButtonClasses: TButtonClasses;
begin
  Result := DarkButtonClasses;
end;

procedure TAngularButtonStyleDark.GetStyleByModalResult(
  const AModalResult: System.UITypes.TModalResult;
  var AStyleClass: TStyledButtonClass;
  var AStyleAppearance: TStyledButtonAppearance);
begin
  //if AModalResult is mrNone, define the defaults of Family
  case AModalResult of
    mrNone     : begin AStyleClass := btn_Basic; AStyleAppearance := FlatAttr; end;
    mrYes      : begin AStyleClass := btn_PrimaryPurple; AStyleAppearance := FlatAttr; end;
    mrNo       : begin AStyleClass := btn_Warn; AStyleAppearance := FlatAttr; end;
    mrOk       : begin AStyleClass := btn_PrimaryPurple; AStyleAppearance := FlatAttr; end;
    mrCancel   : begin AStyleClass := btn_Warn; AStyleAppearance := FlatAttr; end;
    mrAbort    : begin AStyleClass := btn_Warn; AStyleAppearance := RaisedAttr; end;
    mrRetry    : begin AStyleClass := btn_PrimaryPurple; AStyleAppearance := RaisedAttr; end;
    mrIgnore   : begin AStyleClass := btn_PrimaryPurple; AStyleAppearance := RaisedAttr; end;
    mrAll      : begin AStyleClass := btn_PrimaryPurple; AStyleAppearance := RaisedAttr; end;
    mrNoToAll  : begin AStyleClass := btn_Warn; AStyleAppearance := RaisedAttr; end;
    mrYesToAll : begin AStyleClass := btn_PrimaryPurple; AStyleAppearance := RaisedAttr; end;
    mrClose    : begin AStyleClass := btn_Warn; AStyleAppearance := FlatAttr; end;
    mrTryAgain : begin AStyleClass := btn_PrimaryPurple; AStyleAppearance := FlatAttr; end;
    mrContinue : begin AStyleClass := btn_PrimaryPurple; AStyleAppearance := FlatAttr; end;
    mrHelp     : begin AStyleClass := btn_Warn; AStyleAppearance := FlatAttr; end;
  end;
end;

procedure TAngularButtonStyleDark.UpdateAttributes(
  const AFamily: TStyledButtonFamily; const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance; var ANormalStyle, APressedStyle,
  ASelectedStyle, AHotStyle, ADisabledStyle: TStyledButtonAttributes);
var
  LFontColor, LButtonColor: TColor;
  LRaised, LStroked, LBasic: Boolean;
  LPrimaryAccentWarn: Boolean;
begin
  AngularClassToDarkColors(AClass, AAppearance, LFontColor, LButtonColor);

  //Default Style Attributes for Angular Buttons: Flat
  //using Flat Style as base style
  ANormalStyle.DrawType := btRounded;
  ANormalStyle.BorderWidth := 0;
  ANormalStyle.BorderDrawStyle := brdClear;
  ANormalStyle.ButtonDrawStyle := btnSolid;
  ANormalStyle.FontStyle := [fsBold];
  ANormalStyle.ButtonColor := LButtonColor;
  ANormalStyle.FontColor := LFontColor;

  LStroked := SameText(AAppearance, StrokedAttr);
  LRaised := SameText(AAppearance, RaisedAttr);
  LPrimaryAccentWarn :=
    SameText(AClass, btn_PrimaryPink) or
    SameText(AClass, btn_PrimaryPurple) or
    SameText(AClass, btn_AccentBlueGray) or
    SameText(AClass, btn_AccentGreen) or
    SameText(AClass, btn_Warn);
  LBasic := SameText(AAppearance, BasicAttr);

  if LStroked then
  begin
    //Every stroked Buttons has thin border and
    //and no color for the Button
    ANormalStyle.BorderWidth := ANGULAR_STROKED_WIDTH;
    ANormalStyle.BorderDrawStyle := brdSolid;
    ANormalStyle.ButtonDrawStyle := btnClear;
    //Only for Primary, Accent and Warn, Stroked FontColor as Button Color
    if LPrimaryAccentWarn then
      ANormalStyle.FontColor := LButtonColor
    else
      ANormalStyle.FontColor := LFontColor;
  end
  else if LRaised then
  begin
    //Raised Button can be showed with a darker border color
    //from button color, because we don't have "shadow" effect
    ANormalStyle.BorderWidth := ANGULAR_RAISED_WIDTH;
    ANormalStyle.BorderDrawStyle := brdSolid;
    ANormalStyle.BorderColor := DarkenColor(ANormalStyle.ButtonColor, 20);
  end
  else if LBasic then
  begin
    //Basic Buttons has FontColor as Flat button Color
    //But no Color for the Button
    ANormalStyle.ButtonDrawStyle := btnClear;
    //Only for Primary, Accent and Warn, Stroked FontColor as Button Color
    if LPrimaryAccentWarn then
      ANormalStyle.FontColor := LButtonColor
    else
      ANormalStyle.FontColor := LFontColor;
  end;

  //Clone Normal Style to Other Styles
  CloneButtonStyle(ANormalStyle, APressedStyle);
  CloneButtonStyle(ANormalStyle, ASelectedStyle);
  CloneButtonStyle(ANormalStyle, AHotStyle);
  CloneButtonStyle(ANormalStyle, ADisabledStyle);

  if LStroked or LBasic then
  begin
    //Button Hot: Button Color 50% darken of Font
    AHotStyle.ButtonDrawStyle := btnSolid;
    AHotStyle.ButtonColor := DarkenColor(AHotStyle.FontColor, 50);

    //Button Pressed: Button Color 40% darken of Font
    APressedStyle.ButtonDrawStyle := btnSolid;
    APressedStyle.ButtonColor := DarkenColor(APressedStyle.FontColor, 40);

    //Button Selected: Button Color 60% darken of Font
    ASelectedStyle.ButtonDrawStyle := btnSolid;
    ASelectedStyle.ButtonColor := DarkenColor(ASelectedStyle.FontColor, 60);

    //Button Disabled
    ADisabledStyle.FontColor := htmlToColor('#6F6F6F');
  end
  else
  begin
    //Flat and Raised Appearance
    AHotStyle.ButtonColor := DarkenColor(AHotStyle.ButtonColor, 10);

    //Button Focused: botton color Lighten
    ASelectedStyle.ButtonColor :=  LightenColor(ASelectedStyle.ButtonColor, 10);

    //Button Down: botton color Lighten then Focused
    APressedStyle.ButtonColor :=  LightenColor(APressedStyle.ButtonColor, 20);

    //Button Disabled: Gray Button and Font
    ADisabledStyle.BorderWidth := 0;
    ADisabledStyle.ButtonColor := htmlToColor('#424242');
    ADisabledStyle.FontColor := htmlToColor('#6F6F6F');
  end;
end;

initialization
  SetLength(LightButtonClasses, 7);
  LightButtonClasses[0] := btn_Basic;
  LightButtonClasses[1] := btn_PrimaryDeepPurple;
  LightButtonClasses[2] := btn_AccentAmber;
  LightButtonClasses[3] := btn_PrimaryIndigo;
  LightButtonClasses[4] := btn_AccentPink;
  LightButtonClasses[5] := btn_Warn;
  LightButtonClasses[6] := btn_Link;

  SetLength(DarkButtonClasses, 7);
  DarkButtonClasses[0] := btn_Basic;
  DarkButtonClasses[1] := btn_PrimaryPink;
  DarkButtonClasses[2] := btn_AccentBlueGray;
  DarkButtonClasses[3] := btn_PrimaryPurple;
  DarkButtonClasses[4] := btn_AccentGreen;
  DarkButtonClasses[5] := btn_Warn;
  DarkButtonClasses[6] := btn_Link;

  SetLength(ButtonAppearances, 4);
  ButtonAppearances[0] := FlatAttr;
  ButtonAppearances[1] := RaisedAttr;
  ButtonAppearances[2] := BasicAttr;
  ButtonAppearances[3] := StrokedAttr;

  RegisterButtonFamily(TAngularButtonStyleLight.Create);
  RegisterButtonFamily(TAngularButtonStyleDark.Create);

end.
