{******************************************************************************}
{                                                                              }
{  AngulaButtonStyles: Button Styles inspired to Material/angular              }
{  https://v16.material.angular.dev/components/button/overview                      }
{                                                                              }
{  Copyright (c) 2022-2026 (Ethea S.r.l.)                                      }
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

  //Angular Material 3 ("M3") families: the buttons of the current Angular Material
  //(matButton="text|elevated|outlined|filled|tonal"), light and dark colour scheme,
  //colours of the default "azure" palette
  ANGULAR_M3_LIGHT_FAMILY = 'Angular-M3-Light';
  ANGULAR_M3_DARK_FAMILY = 'Angular-M3-Dark';

  //Button Appearance of the Angular Material 3 families
  TextAttr = 'Text';
  ElevatedAttr = 'Elevated';
  OutlinedAttr = 'Outlined';
  FilledAttr = 'Filled';
  TonalAttr = 'Tonal';
  //Floating Action Button: primary container with the shadow of the elevation
  FabAttr = 'Fab';

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

  //Angular Material 3, light colour scheme
  TAngularM3ButtonStyleLight = class(TInterfacedObject, IStyledButtonAttributes)
  private
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

  //Angular Material 3, dark colour scheme
  TAngularM3ButtonStyleDark = class(TInterfacedObject, IStyledButtonAttributes)
  private
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
  M3ButtonClasses: TButtonClasses;
  M3ButtonAppearances: TButtonAppearances;

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
  end
  else
  begin
    //Unknown class: fall back to the Basic light colours instead of leaving the
    //var parameters unassigned (garbage).
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
  else
    GetStyleByModalResult(mrNone, AStyleClass, AStyleAppearance);
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
  ANormalStyle.DrawType := btRoundRect;
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
    //Material stroked outline: rgba(0,0,0,.12) over a light surface. Without
    //an explicit value the border was the clBlack default of a fresh object (F5)
    ANormalStyle.BorderColor := htmlToColor('#E0E0E0');
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
  end
  else
  begin
    //Unknown class: fall back to the Basic dark colours instead of leaving the
    //var parameters unassigned (garbage).
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
  else
    GetStyleByModalResult(mrNone, AStyleClass, AStyleAppearance);
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
  ANormalStyle.DrawType := btRoundRect;
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
    //Material stroked outline: rgba(255,255,255,.12) over a dark surface (F5)
    ANormalStyle.BorderColor := htmlToColor('#494949');
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

{ Angular Material 3 }

type
  //System colours (roles of the Material 3 colour scheme) used by the buttons
  TM3Scheme = record
    Surface: TColor;
    OnSurface: TColor;
    Primary: TColor;
    OnPrimary: TColor;
    PrimaryContainer: TColor;
    OnPrimaryContainer: TColor;
    SecondaryContainer: TColor;
    OnSecondaryContainer: TColor;
    Outline: TColor;
    OutlineVariant: TColor;
  end;

const
  //State layers of the Angular Material 3 buttons (hover/focus/pressed)
  M3_HOVER_OPACITY = 8;    //percent
  M3_FOCUS_OPACITY = 12;
  //Pressed: state layer 12% plus the ripple (12%, still visible while the button is held)
  M3_PRESSED_OPACITY = 22;
  //Disabled: container on-surface 12%, label on-surface 38%
  M3_DISABLED_CONTAINER_OPACITY = 12;
  M3_DISABLED_LABEL_OPACITY = 38;
  //Shadow of the Elevated button (pixels): elevation level 1 at rest, 2 on hover
  M3_ELEVATED_SHADOW = 2;
  M3_ELEVATED_HOT_SHADOW = 3;
  //Shadow of the FAB (elevation level 3 at rest, 4 on hover)
  M3_FAB_SHADOW = 3;
  M3_FAB_HOT_SHADOW = 4;

function M3LightScheme: TM3Scheme;
begin
  //Azure palette: primary 40, secondary 90/30, neutral 98/10, neutral-variant 50/80
  Result.Surface := htmlToColor('#FAF9FD');
  Result.OnSurface := htmlToColor('#1A1B1F');
  Result.Primary := htmlToColor('#005CBB');
  Result.OnPrimary := htmlToColor('#FFFFFF');
  Result.PrimaryContainer := htmlToColor('#D7E3FF');
  Result.OnPrimaryContainer := htmlToColor('#001B3F');
  Result.SecondaryContainer := htmlToColor('#DAE2F9');
  Result.OnSecondaryContainer := htmlToColor('#3E4759');
  Result.Outline := htmlToColor('#74777F');
  Result.OutlineVariant := htmlToColor('#C4C6D0');
end;

function M3DarkScheme: TM3Scheme;
begin
  //Azure palette: primary 80, secondary 30/90, neutral 6/90, neutral-variant 60/30
  Result.Surface := htmlToColor('#121316');
  Result.OnSurface := htmlToColor('#E3E2E6');
  Result.Primary := htmlToColor('#ABC7FF');
  Result.OnPrimary := htmlToColor('#002F65');
  Result.PrimaryContainer := htmlToColor('#00458F');
  Result.OnPrimaryContainer := htmlToColor('#D7E3FF');
  Result.SecondaryContainer := htmlToColor('#3E4759');
  Result.OnSecondaryContainer := htmlToColor('#DAE2F9');
  Result.Outline := htmlToColor('#8E9099');
  Result.OutlineVariant := htmlToColor('#44474E');
end;

//Colour of AFront drawn at APercent opacity over ABack (state layers, disabled colours)
function M3Blend(const ABack, AFront: TColor; const APercent: Integer): TColor;
var
  LBack, LFront: Integer;
  function Channel(const AShift: Integer): Integer;
  begin
    Result := (((LBack shr AShift) and $FF) * (100 - APercent) +
      ((LFront shr AShift) and $FF) * APercent + 50) div 100;
  end;
begin
  LBack := ColorToRGB(ABack);
  LFront := ColorToRGB(AFront);
  Result := TColor(Channel(0) or (Channel(8) shl 8) or (Channel(16) shl 16));
end;

procedure M3SetStateLayer(const AStyle: TStyledButtonAttributes;
  const ABase, ALayer: TColor; const AOpacity: Integer);
begin
  AStyle.ButtonDrawStyle := btnSolid;
  AStyle.ButtonColor := M3Blend(ABase, ALayer, AOpacity);
end;

procedure M3UpdateAttributes(const AScheme: TM3Scheme;
  const AAppearance: TStyledButtonAppearance;
  var ANormalStyle, APressedStyle, ASelectedStyle,
  AHotStyle, ADisabledStyle: TStyledButtonAttributes);
var
  LBack, LFont, LLayer, LOutline: TColor;
  LHasBack, LHasOutline, LElevated, LFab: Boolean;
  LDisabledBack, LDisabledFont: TColor;
begin
  LElevated := False;
  LFab := False;
  //Colour of the container (when it has one), of the label and of the state layer
  LHasBack := False;
  LHasOutline := False;
  LOutline := AScheme.Outline;
  LBack := AScheme.Surface; //the surface under a button without container
  LFont := AScheme.Primary;
  LLayer := AScheme.Primary;
  if SameText(AAppearance, FilledAttr) then
  begin
    LHasBack := True;
    LBack := AScheme.Primary;
    LFont := AScheme.OnPrimary;
    LLayer := AScheme.OnPrimary;
  end
  else if SameText(AAppearance, TonalAttr) or SameText(AAppearance, FabAttr) then
  begin
    LHasBack := True;
    LFab := SameText(AAppearance, FabAttr);
    if LFab then
    begin
      //the FAB of Angular Material uses the primary container
      LBack := AScheme.PrimaryContainer;
      LFont := AScheme.OnPrimaryContainer;
      LLayer := AScheme.OnPrimaryContainer;
    end
    else
    begin
      LBack := AScheme.SecondaryContainer;
      LFont := AScheme.OnSecondaryContainer;
      LLayer := AScheme.OnSecondaryContainer;
    end;
  end
  else if SameText(AAppearance, ElevatedAttr) then
  begin
    //container = surface (as in the Angular Material tokens), with the shadow of the elevation
    LHasBack := True;
    LElevated := True;
    LBack := AScheme.Surface;
  end
  else if SameText(AAppearance, OutlinedAttr) then
    LHasOutline := True;
  //else Text (also for an unknown appearance): no container, no border

  //Normal. Radius: every M3 button is a "full" corner (btRounded)
  ANormalStyle.DrawType := btRounded;
  ANormalStyle.FontStyle := [];
  ANormalStyle.FontColor := LFont;
  //ButtonColor always holds a real colour: the consumers that fill a surface read it
  ANormalStyle.ButtonColor := LBack;
  if LHasBack then
    ANormalStyle.ButtonDrawStyle := btnSolid
  else
    ANormalStyle.ButtonDrawStyle := btnClear;
  if LHasOutline then
  begin
    ANormalStyle.BorderDrawStyle := brdSolid;
    ANormalStyle.BorderWidth := 1;
    ANormalStyle.BorderColor := LOutline;
  end
  else
  begin
    ANormalStyle.BorderDrawStyle := brdClear;
    ANormalStyle.BorderWidth := 0;
    ANormalStyle.BorderColor := LBack;
  end;

  if LElevated then
    ANormalStyle.ShadowSize := M3_ELEVATED_SHADOW
  else if LFab then
    ANormalStyle.ShadowSize := M3_FAB_SHADOW
  else
    ANormalStyle.ShadowSize := 0;

  CloneButtonStyle(ANormalStyle, APressedStyle);
  CloneButtonStyle(ANormalStyle, ASelectedStyle);
  CloneButtonStyle(ANormalStyle, AHotStyle);
  CloneButtonStyle(ANormalStyle, ADisabledStyle);
  //Elevation: higher on hover, none when disabled
  if LElevated then
    AHotStyle.ShadowSize := M3_ELEVATED_HOT_SHADOW
  else if LFab then
    AHotStyle.ShadowSize := M3_FAB_HOT_SHADOW;
  ADisabledStyle.ShadowSize := 0;

  //State layers: the label colour over the container (or over the surface)
  M3SetStateLayer(AHotStyle, LBack, LLayer, M3_HOVER_OPACITY);
  M3SetStateLayer(ASelectedStyle, LBack, LLayer, M3_FOCUS_OPACITY);
  M3SetStateLayer(APressedStyle, LBack, LLayer, M3_PRESSED_OPACITY);
  //The border of Outlined keeps its colour in every state

  //Disabled: container on-surface 12%, label on-surface 38%, outline on-surface 12%
  LDisabledBack := M3Blend(AScheme.Surface, AScheme.OnSurface, M3_DISABLED_CONTAINER_OPACITY);
  LDisabledFont := M3Blend(AScheme.Surface, AScheme.OnSurface, M3_DISABLED_LABEL_OPACITY);
  ADisabledStyle.FontColor := LDisabledFont;
  if LHasBack then
  begin
    ADisabledStyle.ButtonDrawStyle := btnSolid;
    ADisabledStyle.ButtonColor := LDisabledBack;
    ADisabledStyle.BorderDrawStyle := brdClear;
    ADisabledStyle.BorderWidth := 0;
    ADisabledStyle.BorderColor := LDisabledBack;
  end
  else
  begin
    ADisabledStyle.ButtonDrawStyle := btnClear;
    ADisabledStyle.ButtonColor := AScheme.Surface;
    if LHasOutline then
    begin
      ADisabledStyle.BorderDrawStyle := brdSolid;
      ADisabledStyle.BorderWidth := 1;
      ADisabledStyle.BorderColor := LDisabledBack;
    end;
  end;
end;

procedure M3StyleByModalResult(
  const AModalResult: System.UITypes.TModalResult;
  var AStyleClass: TStyledButtonClass;
  var AStyleAppearance: TStyledButtonAppearance);
begin
  //Only the Basic class exists: what changes is the appearance
  AStyleClass := btn_Basic;
  case AModalResult of
    //affirmative answers: filled
    mrNone, mrYes, mrOk, mrAll, mrYesToAll, mrRetry, mrTryAgain, mrContinue:
      AStyleAppearance := FilledAttr;
    //negative answers: outlined
    mrNo, mrCancel, mrAbort, mrNoToAll, mrClose:
      AStyleAppearance := OutlinedAttr;
    //the others: text
    mrIgnore, mrHelp:
      AStyleAppearance := TextAttr;
  else
    AStyleAppearance := FilledAttr;
  end;
end;

{ TAngularM3ButtonStyleLight }

function TAngularM3ButtonStyleLight.ButtonFamilyName: string;
begin
  Result := ANGULAR_M3_LIGHT_FAMILY;
end;

function TAngularM3ButtonStyleLight.GetButtonAppearances: TButtonAppearances;
begin
  Result := M3ButtonAppearances;
end;

function TAngularM3ButtonStyleLight.GetButtonClasses: TButtonClasses;
begin
  Result := M3ButtonClasses;
end;

procedure TAngularM3ButtonStyleLight.GetStyleByModalResult(
  const AModalResult: System.UITypes.TModalResult;
  var AStyleClass: TStyledButtonClass;
  var AStyleAppearance: TStyledButtonAppearance);
begin
  M3StyleByModalResult(AModalResult, AStyleClass, AStyleAppearance);
end;

procedure TAngularM3ButtonStyleLight.UpdateAttributes(
  const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  var ANormalStyle, APressedStyle, ASelectedStyle,
  AHotStyle, ADisabledStyle: TStyledButtonAttributes);
begin
  //single class (Basic): the class does not change the colours
  M3UpdateAttributes(M3LightScheme, AAppearance,
    ANormalStyle, APressedStyle, ASelectedStyle, AHotStyle, ADisabledStyle);
end;

{ TAngularM3ButtonStyleDark }

function TAngularM3ButtonStyleDark.ButtonFamilyName: string;
begin
  Result := ANGULAR_M3_DARK_FAMILY;
end;

function TAngularM3ButtonStyleDark.GetButtonAppearances: TButtonAppearances;
begin
  Result := M3ButtonAppearances;
end;

function TAngularM3ButtonStyleDark.GetButtonClasses: TButtonClasses;
begin
  Result := M3ButtonClasses;
end;

procedure TAngularM3ButtonStyleDark.GetStyleByModalResult(
  const AModalResult: System.UITypes.TModalResult;
  var AStyleClass: TStyledButtonClass;
  var AStyleAppearance: TStyledButtonAppearance);
begin
  M3StyleByModalResult(AModalResult, AStyleClass, AStyleAppearance);
end;

procedure TAngularM3ButtonStyleDark.UpdateAttributes(
  const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  var ANormalStyle, APressedStyle, ASelectedStyle,
  AHotStyle, ADisabledStyle: TStyledButtonAttributes);
begin
  M3UpdateAttributes(M3DarkScheme, AAppearance,
    ANormalStyle, APressedStyle, ASelectedStyle, AHotStyle, ADisabledStyle);
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

  //Angular Material 3: a single class. The Basic/Link/Disabled variants of the
  //documentation page are the same button as <button>, as <a> and disabled: here
  //there is no difference between a button and a link, and Disabled is a state
  SetLength(M3ButtonClasses, 1);
  M3ButtonClasses[0] := btn_Basic;

  SetLength(M3ButtonAppearances, 6);
  M3ButtonAppearances[0] := TextAttr;
  M3ButtonAppearances[1] := ElevatedAttr;
  M3ButtonAppearances[2] := OutlinedAttr;
  M3ButtonAppearances[3] := FilledAttr;
  M3ButtonAppearances[4] := TonalAttr;
  M3ButtonAppearances[5] := FabAttr;

  RegisterButtonFamily(TAngularButtonStyleLight.Create);
  RegisterButtonFamily(TAngularButtonStyleDark.Create);
  RegisterButtonFamily(TAngularM3ButtonStyleLight.Create);
  RegisterButtonFamily(TAngularM3ButtonStyleDark.Create);

end.
