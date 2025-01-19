{******************************************************************************}
{                                                                              }
{  TStyledComponents Demo                                                      }
{  Settings Unit                                                               }
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
unit uSettings;

interface

uses
  System.SysUtils
  , System.Classes
  , VCL.Graphics
  , System.IniFiles
  , System.Generics.Collections
  , Vcl.StandardButtonStyles
  ;

const
  MinfontSize = 9;

type
  TThemeSelection = (tsAsWindows, tsDarkTheme, tsLightTheme);

  TSettings = class
  private
    FIniFile: TIniFile;
    FIsChanged: Boolean;
    FFontSize: Integer;
    FStyleName: string;
    FUseDarkStyle: boolean;
    FFontName: string;
    FActivePageIndex: Integer;
    FThemeSelection: TThemeSelection;
    FButtonDrawRounded: Boolean;
    FToolbarDrawRounded: Boolean;
    FMenuDrawRounded: Boolean;
    function GetUseDarkStyle: Boolean;
    function GetButtonTextColor: TColor;
    class function GetSettingsFileName: string; static;
    procedure SetButtonDrawRounded(const AValue: Boolean);
    procedure SetToolbarDrawRounded(const AValue: Boolean);
    procedure SetMenuDrawRounded(const AValue: Boolean);
    procedure Changed;
    procedure SetActivePageIndex(const AValue: Integer);
    procedure SetFontName(const AValue: string);
    procedure SetFontSize(const AValue: Integer);
    procedure SetStyleName(const AValue: string);
    procedure SetThemeSelection(const Value: TThemeSelection);
  public
    constructor CreateSettings(const ASettingFileName: string);
    destructor Destroy; override;

    class var FSettingsFileName: string;
    class var FSettingsPath: string;
    class property SettingsFileName: string read GetSettingsFileName;

    procedure UpdateSettings(const AFontName: string;
      AFontSize: Integer; AEditorVisible: Boolean);
    procedure ReadSettings; virtual;
    procedure WriteSettings; virtual;

    property IsChanged: Boolean read FIsChanged;
    property UseDarkStyle: Boolean read GetUseDarkStyle;
    property ButtonTextColor: TColor read GetButtonTextColor;
    property FontSize: Integer read FFontSize write SetFontSize;
    property FontName: string read FFontName write SetFontName;
    property StyleName: string read FStyleName write SetStyleName;
    property ActivePageIndex: Integer read FActivePageIndex write SetActivePageIndex;
    property ThemeSelection: TThemeSelection read FThemeSelection write SetThemeSelection;
    property ButtonDrawRounded: Boolean read FButtonDrawRounded write SetButtonDrawRounded;
    property ToolbarDrawRounded: Boolean read FToolbarDrawRounded write SetToolbarDrawRounded;
    property MenuDrawRounded: Boolean read FMenuDrawRounded write SetMenuDrawRounded;
  end;

function IsWindowsAppThemeLight: Boolean;
function DefaultStyleName: string;
function GetTempDirectory: string;
function GetSpecialFolder(const CSIDL: integer): string;

implementation

uses
  Vcl.Controls
  , System.Types
  , System.TypInfo
  , System.Rtti
  , System.StrUtils
  , System.IOUtils
  , Winapi.ShlObj
  , Winapi.Windows
  , Vcl.Themes
  , System.Win.Registry
  ;

function GetSpecialFolder(const CSIDL: integer): string;
var
  lpszPath: PWideChar;
begin
  lpszPath := StrAlloc(MAX_PATH);
  try
    ZeroMemory(lpszPath, MAX_PATH);
    if SHGetSpecialFolderPath(0, lpszPath, CSIDL, False) then
      Result := lpszPath
    else
      Result := GetTempDirectory;
  finally
    StrDispose(lpszPath);
  end;
  Result := IncludeTrailingPathDelimiter(Result);
end;

function GetTempDirectory: string;
var
  lpBuffer: array [0 .. MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, @lpBuffer);
  Result := StrPas(lpBuffer);
end;

function DefaultStyleName: string;
begin
  Result := 'Windows';
end;

function IsWindowsAppThemeLight: Boolean;
var
  LIsLight: Integer;
  Reg: TRegistry;
begin
  LIsLight := 1;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Result := Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize');
    if Result then
    begin
      if Reg.ValueExists('AppsUseLightTheme') then
        LIsLight := Reg.ReadInteger('AppsUseLightTheme');
    end;
  finally
    Reg.Free;
  end;
  Result := LIsLight = 1;
end;

{ TSettings }

procedure TSettings.Changed;
begin
  FIsChanged := True;
end;

constructor TSettings.CreateSettings(const ASettingFileName: string);
begin
  inherited Create;
  FIniFile := TIniFile.Create(ASettingFileName);
  FSettingsFileName := ASettingFileName;
  FSettingsPath := ExtractFilePath(ASettingFileName);
  System.SysUtils.ForceDirectories(FSettingsPath);
  ReadSettings;
end;

destructor TSettings.Destroy;
begin
  FIniFile.UpdateFile;
  FreeAndNil(FIniFile);
  inherited;
end;

function TSettings.GetButtonTextColor: TColor;
var
  LStyleServices: TCustomStyleServices;
begin
  LStyleServices := TStyleManager.Style[Self.StyleName];
  if Assigned(LStyleServices) then
    Result := LStyleServices.GetStyleFontColor(sfButtonTextNormal)
  else
    Result := clBtnText;
end;

class function TSettings.GetSettingsFileName: string;
begin
  Result := FSettingsFileName;
end;

function TSettings.GetUseDarkStyle: Boolean;
begin
  Result := FUseDarkStyle;
end;

procedure TSettings.ReadSettings;
begin
  FFontSize := FIniFile.ReadInteger('Global', 'FontSize', 9);
  FFontName := FIniFile.ReadString('Global', 'FontName', 'Segoe UI');
  FActivePageIndex := FIniFile.ReadInteger('Global', 'ActivePageIndex', 0);
  FStyleName := FIniFile.ReadString('Global', 'StyleName', DefaultStyleName);
  FThemeSelection := TThemeSelection(FIniFile.ReadInteger('Global', 'ThemeSelection', 0));
  FToolbarDrawRounded := FIniFile.ReadBool('Global', 'ToolbarDrawRounded', false);
  FButtonDrawRounded := FIniFile.ReadBool('Global', 'ButtonDrawRounded', false);
  FMenuDrawRounded := FIniFile.ReadBool('Global', 'MenuDrawRounded', false);

  //Select Style by default on Actual Windows Theme
  if FThemeSelection = tsAsWindows then
  begin
    FUseDarkStyle := not IsWindowsAppThemeLight;
  end
  else
    FUseDarkStyle := FThemeSelection = tsDarkTheme;
  FIsChanged := False;
end;

procedure TSettings.UpdateSettings(const AFontName: string;
  AFontSize: Integer; AEditorVisible: Boolean);
begin
  FontSize := AFontSize;
  FontName := AFontName;
  Changed;
end;

procedure TSettings.WriteSettings;
begin
  FIniFile.WriteInteger('Global', 'FontSize', FFontSize);
  FIniFile.WriteString('Global', 'FontName', FFontName);
  FIniFile.WriteString('Global', 'StyleName', FStyleName);
  FIniFile.WriteInteger('Global', 'ActivePageIndex', FActivePageIndex);
  FIniFile.WriteInteger('Global', 'ThemeSelection', Ord(FThemeSelection));
  FIniFile.WriteBool('Global', 'ToolbarDrawRounded', ToolbarDrawRounded);
  FIniFile.WriteBool('Global', 'ButtonDrawRounded', ButtonDrawRounded);
  FIniFile.WriteBool('Global', 'MenuDrawRounded', MenuDrawRounded);
  FIsChanged := False;
end;

procedure TSettings.SetThemeSelection(const Value: TThemeSelection);
begin
  FThemeSelection := Value;
end;

procedure TSettings.SetToolbarDrawRounded(const AValue: Boolean);
begin
  if FToolbarDrawRounded <> AValue then
  begin
    FToolbarDrawRounded := AValue;
    Changed;
  end;
end;

procedure TSettings.SetMenuDrawRounded(const AValue: Boolean);
begin
  if FMenuDrawRounded <> AValue then
  begin
    FMenuDrawRounded := AValue;
    Changed;
  end;
end;

procedure TSettings.SetActivePageIndex(const AValue: Integer);
begin
  if FActivePageIndex <> AValue then
  begin
    FActivePageIndex := AValue;
    Changed;
  end;
end;

procedure TSettings.SetStyleName(const AValue: string);
begin
  if FStyleName <> AValue then
  begin
    FStyleName := AValue;
    Changed;
  end;
end;

procedure TSettings.SetButtonDrawRounded(const AValue: Boolean);
begin
  if FButtonDrawRounded <> AValue then
  begin
    FButtonDrawRounded := AValue;
    Changed;
  end;
end;

procedure TSettings.SetFontName(const AValue: string);
begin
  if FFontName <> AValue then
  begin
    FFontName := AValue;
    Changed;
  end;
end;

procedure TSettings.SetFontSize(const AValue: Integer);
begin
  if FFontSize <> AValue then
  begin
    FFontSize := AValue;
    Changed;
  end;
end;

end.
