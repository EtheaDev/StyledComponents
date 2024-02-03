{******************************************************************************}
{                                                                              }
{       TStyledButtonAttributes: a collection of Rendering attributes          }
{       for Styled Components                                                  }
{                                                                              }
{       Copyright (c) 2022-2024 (Ethea S.r.l.)                                 }
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
unit Vcl.ButtonStylesAttributes;

interface

{$INCLUDE StyledComponents.inc}

uses
  Winapi.Windows
  , Vcl.Graphics
  , System.Classes
  , System.Contnrs
  , System.UITypes
  , System.Types
  , Vcl.Controls
  , Vcl.Buttons
  , Winapi.CommCtrl
  ;

const
  DEFAULT_RADIUS = 6;

resourcestring
  ERROR_FAMILY_NOT_FOUND = 'Styled Button Family "%s" not found';

Type
  //Windows Version
  TWindowsVersion = (wvUndefined, wvWindowsXP, wvWindowsVista, wvWindows7,
    wvWindows8, wvWindows8_1, wvWindows10, wvWindows11);

  //string typed attributes
  TStyledButtonFamily = string;
  TStyledButtonClass = string;
  TStyledButtonAppearance = string;

  //Type of border
  TStyledButtonDrawType = (btRoundRect, btRounded, btRect, btEllipse);
  //Type of Draw for Border
  TBorderDrawStyle = (brdClear, brdSolid); //similar to Pen.psClear and Pen.psSolid
  TButtonDrawStyle = (btnClear, btnSolid); //similar to Brush.bsClear and Brush.bsSolid

  //List of available elements
  TButtonFamilies = array of TStyledButtonFamily;
  TButtonClasses = Array of TStyledButtonClass;
  TButtonAppearances = Array of TStyledButtonAppearance;

  TStyledButtonAttributes = class(TComponent)
  private
    //Custom values
    FCustomDrawType: TStyledButtonDrawType;
    FCustomBorderWidth: Integer;
    FCustomBorderDrawStyle: TBorderDrawStyle;
    FCustomButtonDrawStyle: TButtonDrawStyle;
    FCustomBorderColor: TColor;
    FCustomFontColor: TColor;
    FCustomFontStyle: TFontStyles;
    FCustomButtonColor: TColor;
    FCustomRadius: Integer;

    //Default Values retrieved by Family/Class/Appearance
    FDrawType: TStyledButtonDrawType;
    FBorderWidth: Integer;
    FBorderDrawStyle: TBorderDrawStyle;
    FButtonDrawStyle: TButtonDrawStyle;
    FBorderColor: TColor;
    FFontColor: TColor;
    FFontStyle: TFontStyles;
    FFontName: TFontName;
    FButtonColor: TColor;
    FRadius: Integer;

    FOwnerControl: TControl;
    FHasCustomDrawType: Boolean;
    FHasCustomBorderWidth: Boolean;
    FHasCustomBorderDrawStyle: Boolean;
    FHasCustomButtonDrawStyle: Boolean;
    FHasCustomBorderColor: Boolean;
    FHasCustomFontColor: Boolean;
    FHasCustomFontStyle: Boolean;
    FHasCustomButtonColor: Boolean;
    FHasCustomRadius: Boolean;

    procedure InvalidateControl;
    procedure SetBorderColor(const AValue: TColor);
    procedure SetBorderDrawStyle(const AValue: TBorderDrawStyle);
    procedure SetButtonDrawStyle(const AValue: TButtonDrawStyle);
    procedure SetDrawType(const AValue: TStyledButtonDrawType);
    procedure SetBorderWidth(const AValue: Integer);
    procedure SetButtonColor(const AValue: TColor);
    procedure SetFontColor(const AValue: TColor);
    procedure SetFontStyle(const AValue: TFontStyles);
    procedure SetRadius(const AValue: Integer);
    function GetBorderColor: TColor;
    function GetBorderDrawStyle: TBorderDrawStyle;
    function GetBorderWidth: Integer;
    function GetButtonColor: TColor;
    function GetButtonDrawStyle: TButtonDrawStyle;
    function GetDrawType: TStyledButtonDrawType;
    function GetFontColor: TColor;
    function GetFontStyle: TFontStyles;
    function GetRadius: Integer;
    procedure SetCustomAttributes(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    function HasCustomAttributes: Boolean;
    procedure Assign(ASource: TPersistent); override;
    function PenStyle: TPenStyle;
    function BrushStyle: TBrushStyle;
    function AssignStyledAttributes(const ASource: TStyledButtonAttributes): Boolean;
    procedure ResetCustomAttributes;
    property HasCustomDrawType: Boolean read FHasCustomDrawType;
    property HasCustomBorderWidth: Boolean read FHasCustomBorderWidth;
    property HasCustomBorderDrawStyle: Boolean read FHasCustomBorderDrawStyle;
    property HasCustomButtonDrawStyle: Boolean read FHasCustomButtonDrawStyle;
    property HasCustomBorderColor: Boolean read FHasCustomBorderColor;
    property HasCustomFontColor: Boolean read FHasCustomFontColor;
    property HasCustomFontStyle: Boolean read FHasCustomFontStyle;
    property HasCustomButtonColor: Boolean read FHasCustomButtonColor;
    property HasCustomRadius: Boolean read FHasCustomRadius;
  published
    property DrawType: TStyledButtonDrawType read GetDrawType write SetDrawType stored FHasCustomDrawType;
    property BorderWidth: Integer read GetBorderWidth write SetBorderWidth stored FHasCustomBorderWidth;
    property BorderDrawStyle: TBorderDrawStyle read GetBorderDrawStyle write SetBorderDrawStyle stored FHasCustomBorderDrawStyle;
    property ButtonDrawStyle: TButtonDrawStyle read GetButtonDrawStyle write SetButtonDrawStyle stored FHasCustomButtonDrawStyle;
    property BorderColor: TColor read GetBorderColor write SetBorderColor stored FHasCustomBorderColor;
    property FontColor: TColor read GetFontColor write SetFontColor stored FHasCustomFontColor;
    property FontStyle: TFontStyles read GetFontStyle write SetFontStyle stored FHasCustomFontStyle;
    property ButtonColor: TColor read GetButtonColor write SetButtonColor stored FHasCustomButtonColor;
    property Radius: Integer read GetRadius write SetRadius stored FHasCustomRadius;
    property UseCustomAttributes: Boolean read HasCustomAttributes write SetCustomAttributes stored False;
  end;

  //  Abstraction of Graphic Button Attributes
  IStyledButtonAttributes = interface
    ['{C7BC98EE-D513-46B9-881A-2FDE8DE07786}']
    procedure UpdateAttributes(
      const AFamily:  TStyledButtonFamily;
      const AStyle: TStyledButtonClass;
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

  TButtonFamily = class(TObject)
  private
    FStyleFamily: TStyledButtonFamily;
    FCustomAttributes: IStyledButtonAttributes;
  public
    property StyledAttributes: IStyledButtonAttributes read FCustomAttributes;
  end;

//utilities
function DarkenColor(Color:TColor; Percent:integer):TColor;
function LightenColor(Color:TColor; Percent:integer):TColor;
function HtmlToColor(Color: string): TColor;
function ColortoGrayscale(AColor : TColor): TColor;
function ColorIsLight(Color: TColor): Boolean;
function SameStyledButtonStyle(Style1, Style2: TStyledButtonAttributes): Boolean;
procedure CloneButtonStyle(const ASource: TStyledButtonAttributes;
  var ADest: TStyledButtonAttributes);
function GetWindowsVersion: TWindowsVersion;

//drawing "old-style" with masked bitmap
procedure DrawBitBtnGlyph(ACanvas: TCanvas; ARect: TRect; Kind: Vcl.Buttons.TBitBtnKind;
  AState: TButtonState; AEnabled: Boolean;
  AOriginal: TBitmap; ANumGlyphs: Integer; const ATransparentColor: TColor);

//Draw rectangle and border into Canvas
procedure DrawRect(ACanvas: TCanvas; var ARect: TRect);
//draw Button into Canvas
procedure CanvasDrawShape(const ACanvas: TCanvas; ARect: TRect;
  const ADrawType: TStyledButtonDrawType; const ACornerRadius: Single);
//draw Text into Canvas
procedure CanvasDrawText(const ACanvas: TCanvas; ARect: TRect;
  const AText: string; ABiDiModeFlags: LongInt);
//draw bar and triangle for SplitButton into Canvas
procedure CanvasDrawBarAndTriangle(const ACanvas: TCanvas; const ARect: TRect;
  const AScaleFactor: Single; ABarColor, ATriangleColor: TColor);
//draw Vertical bar into Canvas
procedure CanvasDrawBar(const ACanvas: TCanvas; const ARect: TRect;
  const AScaleFactor: Single; ABarColor: TColor);
//draw a triangle into Canvas
procedure CanvasDrawTriangle(const ACanvas: TCanvas; const ARect: TRect;
  const AScaleFactor: Single; ATriangleColor: TColor);

//ButtonFamily Factory
procedure RegisterButtonFamily(
  const AStyledButtonAttributes: IStyledButtonAttributes);
function GetButtonFamilies: TObjectList;
function GetButtonFamilyClass(const AFamilyName: TStyledButtonFamily): TButtonFamily;
function GetButtonFamilyName(const Index: Integer): TStyledButtonFamily;
function GetButtonClasses(const AFamily: TButtonFamily): TButtonClasses;
function GetButtonAppearances(const AFamily: TButtonFamily): TButtonAppearances;
function GetButtonFamilyClasses(const AFamily: TStyledButtonFamily): TButtonClasses;
function GetButtonFamilyAppearances(const AFamily: TStyledButtonFamily): TButtonAppearances;

function StyleFamilyCheckAttributes(
  const AFamily: TStyledButtonFamily;
  var AClass: TStyledButtonClass;
  var AAppearance: TStyledButtonAppearance;
  out AButtonFamily: TButtonFamily): Boolean;

procedure StyleFamilyUpdateAttributes(
  const AFamily: TStyledButtonFamily;
  var AClass: TStyledButtonClass;
  var AAppearance: TStyledButtonAppearance;
  var ANormalStyle, APressedStyle,
  ASelectedStyle, AHotStyle, ADisabledStyle: TStyledButtonAttributes);

procedure StyleFamilyUpdateAttributesByModalResult(
  const AModalResult: TModalResult;
  const AFamily: TStyledButtonFamily;
  var AClass: TStyledButtonClass;
  var AAppearance: TStyledButtonAppearance);

implementation

uses
  System.Win.Registry
{$ifdef GDIPlusSupport}
  , Winapi.GDIPAPI
  , Winapi.GDIPOBJ
{$endif}
  , System.SysUtils
  , System.Math
  ;

var
  _WindowsVersion: TWindowsVersion;

function SameStyledButtonStyle(Style1, Style2: TStyledButtonAttributes): Boolean;
begin
  Result :=
    (Style1.FDrawType = Style2.FDrawType) and
    (Style1.FBorderWidth = Style2.FBorderWidth) and
    (Style1.FBorderDrawStyle = Style2.FBorderDrawStyle) and
    (Style1.FButtonDrawStyle = Style2.FButtonDrawStyle)  and
    (Style1.FBorderColor = Style2.FBorderColor) and
    (Style1.FFontColor = Style2.FFontColor) and
    (Style1.FFontStyle = Style2.FFontStyle) and
    (Style1.FFontName = Style2.FFontName) and
    (Style1.FButtonColor = Style2.FButtonColor) and
    (Style1.FRadius = Style2.FRadius);
end;

function ColortoGrayscale(AColor : TColor): TColor;
var
  LGray : byte;
begin
  // Ignore reserved color values : "INHERIT" (TColors.SysDefault) and "none" (TColors.SysNone) .
  if (AColor = TColors.SysDefault) or (AColor = TColors.SysNone) then exit(aColor);

  // get the luminance according to https://www.w3.org/TR/AERT/#color-contrast
  LGray  := round((0.299 * GetRValue(aColor)) + (0.587 * GetGValue(aColor)) + (0.114 * GetBValue(aColor)));

  // set the result to the new grayscale color including the alpha info
  Result := (aColor and $FF000000) or rgb(LGray, LGray, LGray);
end;

function HtmlToColor(Color: string): TColor;
begin
  Result := StringToColor('$' + Copy(Color, 6, 2) + Copy(Color, 4, 2) + Copy(Color, 2, 2));
end;

function DarkenColor(Color:TColor; Percent:Integer):TColor;
var r,g,b:Byte;
begin
  Color:=ColorToRGB(Color);
  r:=GetRValue(Color);
  g:=GetGValue(Color);
  b:=GetBValue(Color);
  r:=r-muldiv(r,Percent,100);  //Percent% closer to black
  g:=g-muldiv(g,Percent,100);
  b:=b-muldiv(b,Percent,100);
  result:=RGB(r,g,b);
end;

function LightenColor(Color:TColor; Percent:Integer):TColor;
var r,g,b:Byte;
begin
  Color:=ColorToRGB(Color);
  r:=GetRValue(Color);
  g:=GetGValue(Color);
  b:=GetBValue(Color);
  r:=r+muldiv(255-r,Percent,100); //Percent% closer to white
  g:=g+muldiv(255-g,Percent,100);
  b:=b+muldiv(255-b,Percent,100);
  result:=RGB(r,g,b);
end;

function ColorIsLight(Color: TColor): Boolean;
begin
  Color := ColorToRGB(Color);
  Result := ((Color and $FF) + (Color shr 8 and $FF) +
  (Color shr 16 and $FF))>= $180;
end;

procedure CloneButtonStyle(
  const ASource: TStyledButtonAttributes;
  var ADest: TStyledButtonAttributes);
begin
  ADest.FDrawType := ASource.FDrawType;
  ADest.FBorderWidth := ASource.FBorderWidth;
  ADest.FBorderDrawStyle := ASource.FBorderDrawStyle;
  ADest.FButtonDrawStyle := ASource.FButtonDrawStyle;
  ADest.FBorderColor := ASource.FBorderColor;
  ADest.FFontStyle := ASource.FFontStyle;
  ADest.FFontColor := ASource.FFontColor;
  ADest.FButtonColor := ASource.FButtonColor;
  ADest.FRadius := ASource.FRadius;

  ADest.FCustomDrawType := ASource.FCustomDrawType;
  ADest.FCustomBorderWidth := ASource.FCustomBorderWidth;
  ADest.FCustomBorderDrawStyle := ASource.FCustomBorderDrawStyle;
  ADest.FCustomButtonDrawStyle := ASource.FCustomButtonDrawStyle;
  ADest.FCustomBorderColor := ASource.FCustomBorderColor;
  ADest.FCustomFontStyle := ASource.FCustomFontStyle;
  ADest.FCustomFontColor := ASource.FCustomFontColor;
  ADest.FCustomButtonColor := ASource.FCustomButtonColor;
  ADest.FCustomRadius := ASource.FCustomRadius;

  ADest.FHasCustomDrawType := ASource.FHasCustomDrawType;
  ADest.FHasCustomBorderWidth := ASource.FHasCustomBorderWidth;
  ADest.FHasCustomBorderDrawStyle := ASource.FHasCustomBorderDrawStyle;
  ADest.FHasCustomButtonDrawStyle := ASource.FHasCustomButtonDrawStyle;
  ADest.FHasCustomBorderColor := ASource.FHasCustomBorderColor;
  ADest.FHasCustomFontStyle := ASource.FHasCustomFontStyle;
  ADest.FHasCustomFontColor := ASource.FHasCustomFontColor;
  ADest.FHasCustomButtonColor := ASource.FHasCustomButtonColor;
  ADest.FHasCustomRadius := ASource.FHasCustomRadius;
end;

function GetWindowsVersion: TWindowsVersion;
var
  Reg: TRegistry;
  VersionInfo: TOSVersionInfo;
  LBuildNumber: Integer;
begin
  if _WindowsVersion = wvUndefined then
  begin
    VersionInfo.dwOSVersionInfoSize := sizeOf(TOSVersionInfo);
    Reg := TRegistry.Create;
    Try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      case VersionInfo.dwPlatformID of
        VER_PLATFORM_WIN32_WINDOWS:
          Reg.OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion');
      else
        Reg.OpenKeyReadOnly('\Software\Microsoft\Windows NT\CurrentVersion');
      end;
      LBuildNumber := StrToIntDef(Reg.ReadString('CurrentBuild'), 0);
      if LBuildNumber >= 22000 then
        _WindowsVersion := wvWindows11
      else if LBuildNumber >= 10240 then
        _WindowsVersion := wvWindows10
      else if LBuildNumber >= 9600 then
        _WindowsVersion := wvWindows8_1
      else if LBuildNumber >= 9200 then
        _WindowsVersion := wvWindows8
      else if LBuildNumber >= 7600 then
        _WindowsVersion := wvWindows7
      else if LBuildNumber >= 6000 then
        _WindowsVersion := wvWindowsVista
      else if LBuildNumber >= 2600 then
        _WindowsVersion := wvWindowsXP;
      Reg.CloseKey;
    Finally
      Reg.Free;
    End;
  end;
  Result := _WindowsVersion;
end;

var
  FFamilies: TObjectList;

function GetButtonFamily(const AFamily: TStyledButtonFamily;
  out AButtonFamily: TButtonFamily): Boolean;
var
  I: Integer;
  LButtonFamily: TButtonFamily;
begin
  Result := False;
  for I := 0 to FFamilies.Count -1 do
  begin
    LButtonFamily := FFamilies.Items[I] as TButtonFamily;
    if SameText(LButtonFamily.FStyleFamily, AFamily) then
    begin
      AButtonFamily := LButtonFamily;
      Result := True;
      Exit;
    end;
    AButtonFamily := nil;
  end;
end;

function StyleFamilyCheckAttributes(
  const AFamily: TStyledButtonFamily;
  var AClass: TStyledButtonClass;
  var AAppearance: TStyledButtonAppearance;
  out AButtonFamily: TButtonFamily): Boolean;
var
  I: Integer;
  LClasses: TButtonClasses;
  LAppearances: TButtonAppearances;
  LDefaultClass: TStyledButtonClass;
  LDefaultAppearance: TStyledButtonAppearance;
  LClassFound, LAppearanceFound: Boolean;
begin
  Result := True;
  if GetButtonFamily(AFamily, AButtonFamily) then
  begin
    AButtonFamily.FCustomAttributes.GetStyleByModalResult(mrNone,
       LDefaultClass, LDefaultAppearance);
    //Check AClass
    LClassFound := False;
    LClasses := AButtonFamily.FCustomAttributes.GetButtonClasses;
    for I := 0 to Length(LClasses)-1 do
    begin
      if SameText(LClasses[I], AClass) then
      begin
        AClass := LClasses[I];
        LClassFound := True;
        break;
      end;
    end;
    if not LClassFound then
    begin
      AClass := LDefaultClass;
      Result := False;
    end;

    //Check AAppearance
    LAppearanceFound := False;
    LAppearances := AButtonFamily.FCustomAttributes.GetButtonAppearances;
    for I := 0 to Length(LAppearances)-1 do
    begin
      if SameText(LAppearances[I], AAppearance) then
      begin
        AAppearance := LAppearances[I];
        LAppearanceFound := True;
        break;
      end;
    end;
    if not LAppearanceFound then
    begin
      AAppearance := LDefaultAppearance;
      Result := False;
    end;
  end;
end;

procedure StyleFamilyUpdateAttributes(
  const AFamily: TStyledButtonFamily;
  var AClass: TStyledButtonClass;
  var AAppearance: TStyledButtonAppearance;
  var ANormalStyle, APressedStyle,
  ASelectedStyle, AHotStyle, ADisabledStyle: TStyledButtonAttributes);
var
  LButtonFamily: TButtonFamily;
  LNormalStyle, LPressedStyle, LSelectedStyle, LHotStyle, LDisabledStyle: TStyledButtonAttributes;
begin
  if GetButtonFamily(AFamily, LButtonFamily) then
  begin
    LNormalStyle := TStyledButtonAttributes.Create(nil);
    LPressedStyle := TStyledButtonAttributes.Create(nil);
    LSelectedStyle := TStyledButtonAttributes.Create(nil);
    LHotStyle := TStyledButtonAttributes.Create(nil);
    LDisabledStyle := TStyledButtonAttributes.Create(nil);
    try
      LButtonFamily.FCustomAttributes.UpdateAttributes(
        AFamily, AClass, AAppearance,
        LNormalStyle, LPressedStyle, LSelectedStyle,
        LHotStyle, LDisabledStyle);

      ANormalStyle.AssignStyledAttributes(LNormalStyle);
      APressedStyle.AssignStyledAttributes(LPressedStyle);
      ASelectedStyle.AssignStyledAttributes(LSelectedStyle);
      AHotStyle.AssignStyledAttributes(LHotStyle);
      ADisabledStyle.AssignStyledAttributes(LDisabledStyle);
    finally
      LNormalStyle.Free;
      LPressedStyle.Free;
      LSelectedStyle.Free;
      LHotStyle.Free;
      LDisabledStyle.Free;
    end;
    //Attributes defined with Family/Class/Appearance reset any changes
(*
    ANormalStyle.ResetChanged;
    APressedStyle.ResetChanged;
    ASelectedStyle.ResetChanged;
    AHotStyle.ResetChanged;
    ADisabledStyle.ResetChanged;
*)
  end;
end;

procedure StyleFamilyUpdateAttributesByModalResult(
  const AModalResult: TModalResult;
  const AFamily: TStyledButtonFamily;
  var AClass: TStyledButtonClass;
  var AAppearance: TStyledButtonAppearance);
var
  LButtonFamily: TButtonFamily;
begin
  if GetButtonFamily(AFamily, LButtonFamily) then
  begin
    LButtonFamily.FCustomAttributes.GetStyleByModalResult(
      AModalResult,
      AClass, AAppearance);
  end;
end;

procedure RegisterButtonFamily(
  const AStyledButtonAttributes: IStyledButtonAttributes);
var
  LFamily: TButtonFamily;
begin
  LFamily := TButtonFamily.Create;
  LFamily.FStyleFamily := AStyledButtonAttributes.ButtonFamilyName;
  LFamily.FCustomAttributes := AStyledButtonAttributes;
  FFamilies.Add(LFamily);
end;

function GetButtonFamilies: TObjectList;
begin
  Result := FFamilies;
end;

function GetButtonFamilyClass(const AFamilyName: TStyledButtonFamily): TButtonFamily;
begin
  if not GetButtonFamily(AFamilyName, Result) then
    raise Exception.CreateFmt(ERROR_FAMILY_NOT_FOUND,[AFamilyName]);
end;

function GetButtonClasses(const AFamily: TButtonFamily): TButtonClasses;
begin
  Result := AFamily.FCustomAttributes.GetButtonClasses;
end;

function GetButtonAppearances(const AFamily: TButtonFamily): TButtonAppearances;
begin
  Result := AFamily.FCustomAttributes.GetButtonAppearances;
end;

function GetButtonFamilyName(const Index: Integer): TStyledButtonFamily;
begin
  Result := (FFamilies.Items[Index] as TButtonFamily).FStyleFamily;
end;

function GetButtonFamilyClasses(const AFamily: TStyledButtonFamily): TButtonClasses;
var
  LButtonFamily: TButtonFamily;
begin
  if GetButtonFamily(AFamily, LButtonFamily) then
    Result := LButtonFamily.FCustomAttributes.GetButtonClasses
  else
    raise Exception.CreateFmt(ERROR_FAMILY_NOT_FOUND,[AFamily]);
end;

function GetButtonFamilyAppearances(const AFamily: TStyledButtonFamily): TButtonAppearances;
var
  LButtonFamily: TButtonFamily;
begin
  if GetButtonFamily(AFamily, LButtonFamily) then
    Result := LButtonFamily.FCustomAttributes.GetButtonAppearances
  else
    raise Exception.CreateFmt(ERROR_FAMILY_NOT_FOUND,[AFamily]);
end;

{ TStyledButtonAttributes }

procedure TStyledButtonAttributes.SetCustomAttributes(const Value: Boolean);
begin
  if not Value then
  begin
    ResetCustomAttributes;
    InvalidateControl;
  end;
end;

function TStyledButtonAttributes.HasCustomAttributes: Boolean;
begin
  Result := FHasCustomDrawType or
    FHasCustomBorderWidth or
    FHasCustomBorderDrawStyle or
    FHasCustomButtonDrawStyle or
    FHasCustomBorderColor or
    FHasCustomFontColor or
    FHasCustomFontStyle or
    FHasCustomButtonColor or
    FHasCustomRadius;
end;

function TStyledButtonAttributes.PenStyle: TPenStyle;
begin
  case BorderDrawStyle of
    brdClear: Result := psClear;
  else
    Result := psSolid;
  end;
end;

procedure TStyledButtonAttributes.ResetCustomAttributes;
begin
  FHasCustomDrawType := False;
  FHasCustomBorderWidth := False;
  FHasCustomBorderDrawStyle := False;
  FHasCustomButtonDrawStyle := False;
  FHasCustomBorderColor := False;
  FHasCustomFontColor := False;
  FHasCustomFontStyle := False;
  FHasCustomButtonColor := False;
  FHasCustomRadius := False;
end;

procedure TStyledButtonAttributes.Assign(ASource: TPersistent);
var
  LSource: TStyledButtonAttributes;
begin
  if ASource is TStyledButtonAttributes then
  begin
    LSource := TStyledButtonAttributes(ASource);
    CloneButtonStyle(LSource, Self);
  end
  else
    inherited Assign(ASource);
end;

function TStyledButtonAttributes.AssignStyledAttributes(
  const ASource: TStyledButtonAttributes): Boolean;
begin
  //Assign internal "custom" variable
(*
  FCustomDrawType := ASource.FCustomDrawType;
  FCustomBorderWidth := ASource.FCustomBorderWidth;
  FCustomBorderDrawStyle := ASource.FCustomBorderDrawStyle;
  FCustomButtonDrawStyle := ASource.FCustomButtonDrawStyle;
  FCustomBorderColor := ASource.FCustomBorderColor;
  FCustomFontColor := ASource.FCustomFontColor;
  FCustomFontStyle := ASource.FCustomFontStyle;
  FCustomButtonColor := ASource.FCustomButtonColor;
  FCustomRadius := ASource.FCustomRadius;
*)
  //Assign internal variable
  FDrawType := ASource.FDrawType;
  FBorderWidth := ASource.FBorderWidth;
  FBorderDrawStyle := ASource.FBorderDrawStyle;
  FButtonDrawStyle := ASource.FButtonDrawStyle;
  FBorderColor := ASource.FBorderColor;
  FFontColor := ASource.FFontColor;
  FFontStyle := ASource.FFontStyle;
  FButtonColor := ASource.FButtonColor;
  FRadius := ASource.FRadius;
  Result := True;
end;

function TStyledButtonAttributes.BrushStyle: TBrushStyle;
begin
  case FButtonDrawStyle of
    btnClear: Result := bsClear;
  else
    Result := bsSolid;
  end;
end;

constructor TStyledButtonAttributes.Create(AOwner: TComponent);
begin
  inherited;
  FRadius := DEFAULT_RADIUS;
  FBorderDrawStyle := brdSolid;
  FButtonDrawStyle := btnSolid;

  if AOwner is TControl then
  begin
    FOwnerControl := TControl(AOwner);
    SetSubComponent(True);
  end;
end;

function TStyledButtonAttributes.GetBorderColor: TColor;
begin
  if not HasCustomBorderColor then
    Result := FBorderColor
  else
    Result := FCustomBorderColor;
end;

function TStyledButtonAttributes.GetBorderDrawStyle: TBorderDrawStyle;
begin
  if not HasCustomBorderDrawStyle then
    Result := FBorderDrawStyle
  else
    Result := FCustomBorderDrawStyle;
end;

function TStyledButtonAttributes.GetBorderWidth: Integer;
begin
  if not HasCustomBorderWidth then
    Result := FBorderWidth
  else
    Result := FCustomBorderWidth;
end;

function TStyledButtonAttributes.GetButtonColor: TColor;
begin
  if not HasCustomButtonColor then
    Result := FButtonColor
  else
    Result := FCustomButtonColor;
end;

function TStyledButtonAttributes.GetButtonDrawStyle: TButtonDrawStyle;
begin
  if not HasCustomButtonDrawStyle then
    Result := FButtonDrawStyle
  else
    Result := FCustomButtonDrawStyle;
end;

function TStyledButtonAttributes.GetDrawType: TStyledButtonDrawType;
begin
  if not HasCustomDrawType then
    Result := FDrawType
  else
    Result := FCustomDrawType;
end;

function TStyledButtonAttributes.GetFontColor: TColor;
begin
  if not HasCustomFontColor then
    Result := FFontColor
  else
    Result := FCustomFontColor;
end;

function TStyledButtonAttributes.GetFontStyle: TFontStyles;
begin
  if not HasCustomFontStyle then
    Result := FFontStyle
  else
    Result := FCustomFontStyle;
end;

function TStyledButtonAttributes.GetRadius: Integer;
begin
  if not HasCustomRadius then
    Result := FRadius
  else
    Result := FCustomRadius;
end;

procedure TStyledButtonAttributes.InvalidateControl;
begin
  if Assigned(FOwnerControl) then
    FOwnerControl.Invalidate;
end;

procedure TStyledButtonAttributes.SetBorderColor(const AValue: TColor);
begin
  if Assigned(FOwnerControl) then
  begin
    //Setting a Custom property value
    if FCustomBorderColor <> AValue then
    begin
      FCustomBorderColor := AValue;
      FHasCustomBorderColor := FCustomBorderColor <> FBorderColor;
      InvalidateControl;
    end
  end
  else
  begin
    //Setting a property using StyleFamily, StyleClass and StyleAppearance
    if FBorderColor <> AValue then
    begin
      FBorderColor := AValue;
      InvalidateControl;
    end;
  end;
end;

procedure TStyledButtonAttributes.SetBorderDrawStyle(const AValue: TBorderDrawStyle);
begin
  if Assigned(FOwnerControl) then
  begin
    //Setting a Custom property value
    if FCustomBorderDrawStyle <> AValue then
    begin
      FCustomBorderDrawStyle := AValue;
      FHasCustomBorderDrawStyle := FCustomBorderDrawStyle <> FBorderDrawStyle;
      InvalidateControl;
    end
  end
  else
  begin
    //Setting a property using StyleFamily, StyleClass and StyleAppearance
    if FBorderDrawStyle <> AValue then
    begin
      FBorderDrawStyle := AValue;
      InvalidateControl;
    end;
  end;
end;

procedure TStyledButtonAttributes.SetButtonDrawStyle(const AValue: TButtonDrawStyle);
begin
  if Assigned(FOwnerControl) then
  begin
    //Setting a Custom property value
    if FCustomButtonDrawStyle <> AValue then
    begin
      FCustomButtonDrawStyle := AValue;
      FHasCustomButtonDrawStyle := FCustomButtonDrawStyle <> FButtonDrawStyle;
      InvalidateControl;
    end
  end
  else
  begin
    //Setting a property using StyleFamily, StyleClass and StyleAppearance
    if FButtonDrawStyle <> AValue then
    begin
      FButtonDrawStyle := AValue;
      InvalidateControl;
    end;
  end;
end;

procedure TStyledButtonAttributes.SetDrawType(const AValue: TStyledButtonDrawType);
begin
  if Assigned(FOwnerControl) then
  begin
    //Setting a Custom property value
    if FCustomDrawType <> AValue then
    begin
      FCustomDrawType := AValue;
      FHasCustomDrawType := FCustomDrawType <> FDrawType;
      InvalidateControl;
    end
  end
  else
  begin
    //Setting a property using StyleFamily, StyleClass and StyleAppearance
    if FDrawType <> AValue then
    begin
      FDrawType := AValue;
      InvalidateControl;
    end;
  end;
end;

procedure TStyledButtonAttributes.SetBorderWidth(const AValue: Integer);
begin
  if Assigned(FOwnerControl) then
  begin
    //Setting a Custom property value
    if FCustomBorderWidth <> AValue then
    begin
      FCustomBorderWidth := AValue;
      FHasCustomBorderWidth := FCustomBorderWidth <> FBorderWidth;
      InvalidateControl;
    end
  end
  else
  begin
    //Setting a property using StyleFamily, StyleClass and StyleAppearance
    if FBorderWidth <> AValue then
    begin
      FBorderWidth := AValue;
      InvalidateControl;
    end;
  end;
end;

procedure TStyledButtonAttributes.SetButtonColor(const AValue: TColor);
begin
  if Assigned(FOwnerControl) then
  begin
    //Setting a Custom property value
    if FCustomButtonColor <> AValue then
    begin
      FCustomButtonColor := AValue;
      FHasCustomButtonColor := FCustomButtonColor <> FButtonColor;
      InvalidateControl;
    end
  end
  else
  begin
    //Setting a property using StyleFamily, StyleClass and StyleAppearance
    if FButtonColor <> AValue then
    begin
      FButtonColor := AValue;
      InvalidateControl;
    end;
  end;
end;

procedure TStyledButtonAttributes.SetFontColor(const AValue: TColor);
begin
  if Assigned(FOwnerControl) then
  begin
    //Setting a Custom property value
    if FCustomFontColor <> AValue then
    begin
      FCustomFontColor := AValue;
      FHasCustomFontColor := FCustomFontColor <> FFontColor;
      InvalidateControl;
    end
  end
  else
  begin
    //Setting a property using StyleFamily, StyleClass and StyleAppearance
    if FFontColor <> AValue then
    begin
      FFontColor := AValue;
      InvalidateControl;
    end;
  end;
end;

procedure TStyledButtonAttributes.SetFontStyle(const AValue: TFontStyles);
begin
  if Assigned(FOwnerControl) then
  begin
    //Setting a Custom property value
    if FCustomFontStyle <> AValue then
    begin
      FCustomFontStyle := AValue;
      FHasCustomFontStyle := FCustomFontStyle <> FFontStyle;
      InvalidateControl;
    end
  end
  else
  begin
    //Setting a property using StyleFamily, StyleClass and StyleAppearance
    if FFontStyle <> AValue then
    begin
      FFontStyle := AValue;
      InvalidateControl;
    end;
  end;
end;

procedure TStyledButtonAttributes.SetRadius(const AValue: Integer);
begin
  if Assigned(FOwnerControl) then
  begin
    //Setting a Custom property value
    if FCustomRadius <> AValue then
    begin
      FCustomRadius := AValue;
      FHasCustomRadius := FCustomRadius <> FRadius;
      InvalidateControl;
    end
  end
  else
  begin
    //Setting a property using StyleFamily, StyleClass and StyleAppearance
    if FRadius <> AValue then
    begin
      FRadius := AValue;
      InvalidateControl;
    end;
  end;
end;

procedure AdjustCanvasRect(const ACanvas: TCanvas;
  var ARect: TRect; ADrawingRect: Boolean);
var
  LWidth: Integer;
begin
  if not ADrawingRect then
  begin
    //Drawing RoundRect with border
    LWidth := (ACanvas.Pen.Width) div 2;
    InflateRect(ARect, -LWidth, -LWidth);
    if not Odd(ACanvas.Pen.Width) then
    begin
      ARect.Width := ARect.Width +1;
      ARect.Height := ARect.Height +1;
    end;
  end
  else
  begin
    if ACanvas.Pen.Style <> psClear then
    begin
      //Reduce Canvas to draw a Square of Pen.Width
      LWidth := ACanvas.Pen.Width div 2;
      InflateRect(ARect, -LWidth, -LWidth);
      if not Odd(ACanvas.Pen.Width) then
      begin
        ARect.Width := ARect.Width +1;
        ARect.Height := ARect.Height +1;
      end;
    end;
  end;
  if ARect.Height < 2 then
    ARect.Height := 2;
  if ARect.Width < 2 then
    ARect.Width := 2;
end;

{$ifdef GDIPlusSupport}
function GetRoundRectangle(ARectangle: TGPRectF;
  ARadius: Single): TGPGraphicsPath;
var
  LPath : TGPGraphicsPath;
  l, t, w, h, d : Single;
begin
  LPath := TGPGraphicsPath.Create;
  l := ARectangle.X;
  t := ARectangle.y;
  w := ARectangle.Width;
  h := ARectangle.Height;
  d := ARadius / 2;

  // the lines beween the arcs are automatically added by the path
  LPath.AddArc(l, t, d, d, 180, 90); // topleft
  LPath.AddArc(l + w - d, t, d, d, 270, 90); // topright
  LPath.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
  LPath.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
  LPath.CloseFigure();
  result := LPath;
end;

function GPColor(AColor: TColor): TGPColor;
var
  ColRef: COLORREF;
begin
  ColRef := ColorToRGB(AColor);
  Result := MakeColor(GetRValue(ColRef), GetGValue(ColRef),
  GetBValue(ColRef));
end;
{$ifend}

const //Same as Vcl.Buttons
  BitBtnResNames: array[TBitBtnKind] of PChar = (
    nil, 'BBOK', 'BBCANCEL', 'BBHELP', 'BBYES', 'BBNO', 'BBCLOSE',
    'BBABORT', 'BBRETRY', 'BBIGNORE', 'BBALL');

procedure DrawBitBtnGlyph(ACanvas: TCanvas; ARect: TRect; Kind: Vcl.Buttons.TBitBtnKind;
  AState: TButtonState; AEnabled: Boolean;
  AOriginal: TBitmap; ANumGlyphs: Integer; const ATransparentColor: TColor);
const
  ROP_DSPDxax = $00E20746;
var
  IL: TImageList;
  LResName: String;
  LOriginal, TmpImage, MonoBmp, DDB: TBitmap;
  LNumGlyphs: Integer;
  IWidth, IHeight: Integer;
  IRect, ORect: TRect;
  I: TButtonState;
  DestDC: HDC;
  LIndex: Integer;
  {$IFDEF D10_4+}
  LImage: TWicImage;
  {$ENDIF}
begin
  if not AEnabled then
    AState := bsDisabled;
  LIndex := -1;
  IL := nil;
  TmpImage := nil;
  LOriginal := nil;
  try
    if Kind = bkCustom then
    begin
      if ANumGlyphs = 0 then
        Exit;
      LOriginal := AOriginal;
      LNumGlyphs := ANumGlyphs;
    end
    else
    begin
      //Load image from resources by Kind
      LResName := BitBtnResNames[Kind];
      {$IFDEF D10_4+}
      LImage := TWicImage.Create;
      try
        LImage.InterpolationMode := wipmHighQualityCubic;
        LImage.LoadFromResourceName(HInstance, LResName);
        ACanvas.StretchDraw(ARect, LImage);
        Exit;
      finally
        LImage.Free;
      end;
      {$ELSE}
        LOriginal := TBitmap.Create;
        LNumGlyphs := 2;
        LOriginal.PixelFormat := pf32bit;
        LOriginal.LoadFromResourceName(HInstance, LResName);
      {$ENDIF}
    end;
    if (LOriginal.Width = 0) or (LOriginal.Height = 0) then
      Exit;
    IWidth := LOriginal.Width div LNumGlyphs;
    IHeight := LOriginal.Height;
    TmpImage := TBitmap.Create;
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IL := TImageList.CreateSize(TmpImage.Width, TmpImage.Height);
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage.Canvas.Brush.Color := clBtnFace;
    TmpImage.Palette := CopyPalette(LOriginal.Palette);
    I := AState;
    if Ord(I) >= LNumGlyphs then I := bsUp;
    ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);
    case AState of
      bsUp, bsDown,
      bsExclusive:
        begin
          TmpImage.Canvas.CopyRect(IRect, LOriginal.Canvas, ORect);
          if LOriginal.TransparentMode = tmFixed then
            LIndex := IL.AddMasked(TmpImage, ATransparentColor)
          else
            LIndex := IL.AddMasked(TmpImage, clDefault);
          IL.Masked := True;
        end;
      bsDisabled:
        begin
          MonoBmp := nil;
          DDB := nil;
          try
            MonoBmp := TBitmap.Create;
            DDB := TBitmap.Create;
            DDB.Assign(LOriginal);
            DDB.HandleType := bmDDB;
            if ANumGlyphs > 1 then
            with TmpImage.Canvas do
            begin    { Change white & gray to clBtnHighlight and clBtnShadow }
              CopyRect(IRect, DDB.Canvas, ORect);
              MonoBmp.Monochrome := True;
              MonoBmp.Width := IWidth;
              MonoBmp.Height := IHeight;

              { Convert white to clBtnHighlight }
              DDB.Canvas.Brush.Color := clWhite;
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnHighlight;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert gray to clBtnShadow }
              DDB.Canvas.Brush.Color := clGray;
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnShadow;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert transparent color to clBtnFace }
              DDB.Canvas.Brush.Color := ColorToRGB(ATransparentColor);
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnFace;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
            end
            else
            begin
              { Create a disabled version }
              with MonoBmp do
              begin
                Assign(LOriginal);
                HandleType := bmDDB;
                Canvas.Brush.Color := clBlack;
                Width := IWidth;
                if Monochrome then
                begin
                  Canvas.Font.Color := clWhite;
                  Monochrome := False;
                  Canvas.Brush.Color := clWhite;
                end;
                Monochrome := True;
              end;
              with TmpImage.Canvas do
              begin
                Brush.Color := clBtnFace;
                FillRect(IRect);
                Brush.Color := clBtnHighlight;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 1, 1, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
                Brush.Color := clBtnShadow;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              end;
            end;
          finally
            DDB.Free;
            MonoBmp.Free;
          end;
          LIndex := IL.AddMasked(TmpImage, clDefault);
        end;
    end;
    ImageList_DrawEx(IL.Handle, LIndex, ACanvas.Handle, ARect.Left, ARect.Top, 0, 0,
      clNone, clNone, ILD_Transparent);
  finally
    TmpImage.Free;
    if Kind <> bkCustom then
      LOriginal.Free;
    IL.Free;
  end;
end;

procedure CanvasDrawBar(const ACanvas: TCanvas; const ARect: TRect;
  const AScaleFactor: Single; ABarColor: TColor);
var
  Points2: array [0..1] of TPoint;
  LRect: TRect;
begin
  //Draw vertical bar
  ACanvas.Pen.Color := ABarColor;
  LRect := Rect(ARect.Left-2,ARect.Top+2,ARect.Right-2,ARect.Bottom-2);
  Points2[0] := Point(ARect.Left -1, ARect.Top + ACanvas.Pen.Width);
  Points2[1] := Point(ARect.Left -1, ARect.Bottom - ACanvas.Pen.Width);
  ACanvas.Polyline(Points2);
end;

procedure CanvasDrawTriangle(const ACanvas: TCanvas; const ARect: TRect;
  const AScaleFactor: Single; ATriangleColor: TColor);
var
  LWidth: Integer;
  LHeight: Integer;
  LMargin: Integer;
  Points3: array [0..2] of TPoint;
  LRect: TRect;
begin
  //Draw triangle
  LHeight := Round(4 * AScaleFactor);
  LMargin := (ARect.Height - LHeight) div 2;
  ACanvas.Pen.Color := ATriangleColor;
  LRect := ARect;
  LWidth  := LRect.Width - 8;
  LRect.Left := ARect.Left + 2;
  LRect.Right := LRect.Left + LWidth - 2;
  LRect.Top := LMargin;
  LRect.Bottom := LRect.Top + LHeight;
  Points3[0] := Point(LRect.Left + LWidth, LRect.Top);
  Points3[1] := Point(LRect.Left, LRect.Top);
  Points3[2] := Point(LRect.Left + LWidth div 2, LRect.Bottom);
  ACanvas.Brush.Color := ACanvas.Pen.Color;
  ACanvas.Pen.Width := 1;
  ACanvas.Polygon(Points3);
end;

procedure CanvasDrawBarAndTriangle(const ACanvas: TCanvas; const ARect: TRect;
  const AScaleFactor: Single; ABarColor, ATriangleColor: TColor);
begin
  //Draw vertical bar
  CanvasDrawBar(ACanvas, ARect, AScaleFactor, ABarColor);

  //Draw triangle
  CanvasDrawTriangle(ACanvas, ARect, AScaleFactor, ATriangleColor);
end;

{$ifdef GDIPlusSupport}
procedure CanvasDrawShape(const ACanvas: TCanvas; ARect: TRect;
  const ADrawType: TStyledButtonDrawType; const ACornerRadius: Single);
var
  LGraphics: TGPGraphics;
  LPen: TGPPen;
  LBrush: TGPBrush;
  LButtonColor, LPenColor: TGPColor;
  LRect: TGPRectF;
  LPath: TGPGraphicsPath;
  LBorderWidth: Single;
  X, Y, W, H: Single;
  LCornerRadius: Single;

  procedure GPInflateRectF(var ARect: TGPRectF;
    const AValue: Single);
  begin
    ARect.X := ARect.X + (AValue / 2);
    ARect.Y := ARect.Y + (AValue / 2);
    ARect.Width := ARect.width - AValue -1;
    ARect.Height := ARect.Height - AValue -1;
  end;

begin
  LGraphics := nil;
  LPen := nil;
  try
    X := ARect.Left;
    Y := ARect.Top;
    W := ARect.Width;
    H := ARect.Height;
    LRect := Winapi.GDIPAPI.MakeRect(X, Y, W, H);
    LBorderWidth := ACanvas.Pen.Width;
    LGraphics := TGPGraphics.Create(ACanvas.Handle);
    LGraphics.SetSmoothingMode(SmoothingModeAntiAlias);
    LPenColor := GPColor(ACanvas.Pen.Color);
    LButtonColor := GPColor(ACanvas.Brush.Color);

    if ACanvas.Pen.Style = psClear then
      LPen := TGPPen.Create(TAlphaColorRec.Null, LBorderWidth)
    else
      LPen := TGPPen.Create(LPenColor, LBorderWidth);

    if (ADrawType in [btRect]) then
    begin
      DrawRect(ACanvas, ARect);
    end
    else if (ADrawType in [btRounded, btRoundRect]) then
    begin
      //Reduce canvas to draw a rounded rectangle of Pen Width
      GPInflateRectF(LRect, LBorderWidth);
      if ADrawType = btRoundRect then
        LCornerRadius := ACornerRadius //Drawing a Rounded Rect
      else
        LCornerRadius := LRect.Height - LBorderWidth; //Drawing a Rounded Button
      LPath := GetRoundRectangle(LRect, LCornerRadius*2);
      if ACanvas.Brush.Style = bsSolid then
      begin
        LBrush := TGPSolidBrush.Create(LButtonColor);
        LGraphics.FillPath(LBrush, LPath);
      end;
      LGraphics.DrawPath(LPen, LPath);
    end
    else
    begin
      //Reduce canvas
      GPInflateRectF(LRect, LBorderWidth);
      //Drawing Circle or Ellipsis
      if ACanvas.Brush.Style = bsSolid then
      begin
        LBrush := TGPSolidBrush.Create(LButtonColor);
        LGraphics.FillEllipse(LBrush, LRect);
      end;
      LGraphics.DrawEllipse(LPen, LRect);
    end;
  finally
    LGraphics.Free;
    LPen.Free;
  end;
end;
{$else}
procedure CanvasDrawShape(const ACanvas: TCanvas; ARect: TRect;
  const ADrawType: TStyledButtonDrawType; const ACornerRadius: Single);
var
  LCornerRadius, LBorderWidth: Integer;
begin
  LBorderWidth := ACanvas.Pen.Width;
  if ADrawType in [btRounded, btRoundRect] then
  begin
    AdjustCanvasRect(ACanvas, ARect, False);
    if ADrawType = btRoundRect then
      LCornerRadius := Round(ACornerRadius*2) //Drawing a Rounded Rect
    else
      LCornerRadius := ARect.Height - LBorderWidth; //Drawing a Rounded Button
    ACanvas.RoundRect(ARect, Round(LCornerRadius), LCornerRadius);
  end
  else if ADrawType in [btRect] then
  begin
    AdjustCanvasRect(ACanvas, ARect, True);
    if ACanvas.Brush.Style = bsSolid then
      ACanvas.FillRect(ARect);
    ACanvas.Rectangle(ARect);
  end
  else
  begin
    //Drawing Circle or Ellipsis
    ACanvas.Ellipse(ARect.Left, ARect.Top,
      ARect.Left + ARect.Width, ARect.Top + ARect.Height);
  end;
end;
{$endif}

{$ifdef DrawRectWithGDIPlus}
procedure DrawRect(ACanvas: TCanvas; var ARect: TRect);
var
  LGraphics: TGPGraphics;
  LBrush: TGPBrush;
  LPen: TGPPen;
  LButtonColor, LPenColor: TGPColor;
  LBorderWidth: Integer;
  LRect: TGPRectF;
  X, Y, W, H: Single;

  procedure GPInflateRectF(var ARect: TGPRectF;
    const AValue: Single);
  begin
    ARect.X := ARect.X + (AValue / 2);
    ARect.Y := ARect.Y + (AValue / 2);
    ARect.Width := ARect.width - AValue;
    ARect.Height := ARect.Height - AValue;
  end;

begin
  LGraphics := nil;
  LBrush := nil;
  LPen := nil;
  try
    X := ARect.Left;
    Y := ARect.Top;
    W := ARect.Width;
    H := ARect.Height;
    LGraphics := TGPGraphics.Create(ACanvas.Handle);
    LPenColor := GPColor(ACanvas.Pen.Color);
    LButtonColor := GPColor(ACanvas.Brush.Color);
    LBrush := TGPSolidBrush.Create(LButtonColor);
    LBorderWidth := ACanvas.Pen.Width;
    if ACanvas.Pen.Style = psClear then
      LPen := TGPPen.Create(TAlphaColorRec.Null, LBorderWidth)
    else
      LPen := TGPPen.Create(LPenColor, LBorderWidth);
    //Reduce canvas to draw a rounded rectangle of Pen Width
    LRect := Winapi.GDIPAPI.MakeRect(X, Y, W, H);
    GPInflateRectF(LRect, LBorderWidth);
    //GDI+ equivalent of FillRect and Rectangle
    LGraphics.FillRectangle(LBrush, X, Y, W, H);
    LGraphics.DrawRectangle(LPen, LRect);
  finally
    LBrush.Free;
    LGraphics.Free;
    LPen.Free;
  end;
end;
{$else}
procedure DrawRect(ACanvas: TCanvas; var ARect: TRect);
begin
  //Drawing Rectangular button (no need to GDI+)
  AdjustCanvasRect(ACanvas, ARect, True);
  if ACanvas.Brush.Style = bsSolid then
    ACanvas.FillRect(ARect);
  ACanvas.Rectangle(ARect);
end;
{$endif}


{$ifdef DrawTextWithGDIPlus}
function FontStyleToGDI(AFont: TFont): TFontStyle;
begin
  Result := FontStyleRegular;
  if fsBold in AFont.Style then
    Result := Result + FontStyleBold;
  if fsItalic in AFont.Style then
    Result := Result + FontStyleItalic;
  if fsUnderline in AFont.Style then
    Result := Result + FontStyleUnderline;
  if fsStrikeOut in AFont.Style then
    Result := Result + FontStyleStrikeout;
end;

procedure CanvasDrawText(const ACanvas: TCanvas; ARect: TRect;
  const AText: string; ABiDiModeFlags: LongInt);
var
  LGraphics: TGPGraphics;
  LFontFamily: TGPFontFamily;
  LFont: TGPFont;
  LFontStyle: TFontStyle;
  LSolidBrush: TGPSolidBrush;
  LFontColor: TGPColor;
  LPointF: TGPPointF;
  X,Y: Single;
  R: TRectF;
begin
  LGraphics := nil;
  LFontFamily := nil;
  LFont := nil;
  try
    LGraphics := TGPGraphics.Create(ACanvas.Handle);
    LFontFamily := TGPFontFamily.Create(ACanvas.Font.Name);
    LFontStyle := FontStyleToGDI(ACanvas.Font);
    LFont := TGPFont.Create(LFontFamily, -ACanvas.Font.Height,
      LFontStyle, UnitPixel);
    LFontColor := GPColor(ACanvas.Font.Color);
    LSolidBrush := TGPSolidBrush.Create(LFontColor);
    X := ARect.Left-1;
    Y := ARect.Top-1;
    LPointF := MakePoint(X, Y);
    LGraphics.DrawString(AText, Length(AText), LFont, LPointF, LSolidBrush);
  finally
    LGraphics.Free;
    LFontFamily.Free;
    LFont.Free;
  end;
end;
{$else}
procedure CanvasDrawText(const ACanvas: TCanvas; ARect: TRect;
  const AText: string; ABiDiModeFlags: LongInt);
begin
  Winapi.Windows.DrawText(ACanvas.Handle, PChar(AText),
    Length(AText), ARect, ABiDiModeFlags);
end;
{$endif}

initialization
  _WindowsVersion := wvUndefined;
  FFamilies := TObjectList.Create(True);

finalization
  FFamilies.Free;

end.
