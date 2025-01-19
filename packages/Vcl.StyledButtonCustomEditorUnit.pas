{******************************************************************************}
{                                                                              }
{  StyledButton Attributes Editor: Component editor for Styled Button          }
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
unit Vcl.StyledButtonCustomEditorUnit;

interface

{$INCLUDE ..\Source\StyledComponents.inc}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Actions,
  Vcl.ActnList,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.StyledButton,
  Vcl.ButtonStylesAttributes,
  Vcl.StandardButtonStyles,
  Vcl.BootstrapButtonStyles,
  Vcl.AngularButtonStyles,
  Vcl.ColorButtonStyles,
  Vcl.ImgList;

type
  TStyledButtonCustomEditor = class(TForm)
    BottomPanel: TPanel;
    OKButton: TStyledButton;
    ApplyButton: TStyledButton;
    CancelButton: TStyledButton;
    HelpButton: TStyledButton;
    paTop: TPanel;
    ActualGroupBox: TGroupBox;
    NewGroupBox: TGroupBox;
    SourceButton: TStyledGraphicButton;
    DestButton: TStyledGraphicButton;
    AttributesPanel: TPanel;
    AttributesGroupBox: TGroupBox;
    StyleDrawTypeLabel: TLabel;
    StyleRadiusLabel: TLabel;
    EnabledCheckBox: TCheckBox;
    StyleDrawTypeComboBox: TComboBox;
    RadiusTrackBar: TTrackBar;
    FlatButtonCheckBox: TCheckBox;
    TabControl: TTabControl;
    ScrollBox: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure paTopResize(Sender: TObject);
    procedure StyleDrawTypeComboBoxSelect(Sender: TObject);
    procedure EnabledCheckBoxClick(Sender: TObject);
    procedure DestButtonClick(Sender: TObject);
    procedure RadiusTrackBarChange(Sender: TObject);
    procedure FlatButtonCheckBoxClick(Sender: TObject);
  private
    FUpdating: Boolean;
    FStyledButtonRender: TStyledButtonRender;
    FStyledButton: TControl;
    FCustomStyleDrawType: Boolean;
    procedure ApplyStyle;
    procedure InitGUI;
    procedure UpdateDestFromGUI;
    procedure UpdateSizeGUI;
  protected
    procedure Loaded; override;
  public
    property CustomStyleDrawType: Boolean read FCustomStyleDrawType;
  end;

var
  StyledButtonCustomEditor: TStyledButtonCustomEditor;

function EditStyledButtonAttributes(const AButton: TControl): Boolean; overload;
function EditStyledButtonAttributes(const AButton: TStyledGraphicButton): Boolean; overload;
function EditStyledButtonAttributes(const AButton: TStyledButton): Boolean; overload;
function EditStyledButtonAttributes(const AButtonRender: TStyledButtonRender): Boolean; overload;

implementation

{$R *.dfm}

uses
  Vcl.Themes
  , Vcl.Graphics
  //WARNING: you must define this directive to use this unit outside the IDE
{$IFNDEF UseStyledCompEditorsAtRunTime}
  , ToolsAPI
  , BrandingAPI
  {$IF (CompilerVersion >= 32.0)}, IDETheme.Utils{$IFEND}
{$ENDIF}
  , Winapi.ShellAPI
  , System.Contnrs
  , Vcl.StyledCmpStrUtils
  , System.TypInfo
  ;

var
  SavedBounds: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

function EditStyledButtonAttributes(const AButton: TControl): Boolean;
begin
  if AButton is TStyledGraphicButton then
    Result := EditStyledButtonAttributes(TStyledGraphicButton(AButton))
  else if AButton is TStyledButton then
    Result := EditStyledButtonAttributes(TStyledButton(AButton))
  else
    raise Exception.CreateFmt('Cannot Edit Control "%s"', [AButton.Name]);
end;

function EditStyledButtonAttributes(const AButton: TStyledGraphicButton): Boolean;
begin
  Result := EditStyledButtonAttributes(AButton.Render);
end;

function EditStyledButtonAttributes(const AButton: TStyledButton): Boolean;
begin
  Result := EditStyledButtonAttributes(AButton.Render);
end;

function EditStyledButtonAttributes(const AButtonRender: TStyledButtonRender): Boolean;
var
  LEditor: TStyledButtonCustomEditor;
begin
  LEditor := TStyledButtonCustomEditor.Create(nil);
  with LEditor do
  begin
    try
      FStyledButtonRender := AButtonRender;
      FStyledButton := FStyledButtonRender.OwnerControl;
      paTop.Height := SourceButton.Top + AButtonRender.Height + SourceButton.Top;
      AButtonRender.AssignStyleTo(SourceButton.Render);
      SourceButton.Enabled := AButtonRender.Enabled;
      SourceButton.Caption := AButtonRender.Caption;
      SourceButton.Hint := AButtonRender.Hint;
      SourceButton.Width := AButtonRender.Width;
      SourceButton.Height := AButtonRender.Height;
      SourceButton.Flat := AButtonRender.Flat;
      SourceButton.Style := AButtonRender.Style;

      AButtonRender.AssignStyleTo(DestButton.Render);
      DestButton.Enabled := AButtonRender.Enabled;
      DestButton.Width := AButtonRender.Width;
      DestButton.Height := AButtonRender.Height;
      DestButton.Caption := AButtonRender.Caption;
      DestButton.Hint := AButtonRender.Hint;
      DestButton.Flat := AButtonRender.Flat;
      DestButton.Style := AButtonRender.Style;

      Result := ShowModal = mrOk;
      SavedBounds := BoundsRect;
    finally
      Free;
    end;
  end;
end;

{ TStyledButtonCustomEditor }

procedure TStyledButtonCustomEditor.ApplyStyle;
begin
  Screen.Cursor := crHourglass;
  try
    DestButton.Render.SetCustomStyleDrawType(FCustomStyleDrawType);
    DestButton.AssignStyleTo(FStyledButtonRender);
    FStyledButton.Enabled := DestButton.Enabled;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TStyledButtonCustomEditor.ApplyButtonClick(Sender: TObject);
begin
  ApplyStyle;
  UpdateDestFromGUI;
end;

procedure TStyledButtonCustomEditor.DestButtonClick(Sender: TObject);
begin
  ; //Do nothing
end;

procedure TStyledButtonCustomEditor.EnabledCheckBoxClick(Sender: TObject);
begin
  UpdateDestFromGUI;
end;

procedure TStyledButtonCustomEditor.FormCreate(Sender: TObject);
{$IFNDEF UseStyledCompEditorsAtRunTime}
  {$IF (CompilerVersion >= 32.0)}
  var
    LStyle: TCustomStyleServices;
  {$IFEND}
{$ENDIF}
begin
{$IFNDEF UseStyledCompEditorsAtRunTime}
  {$IF (CompilerVersion >= 32.0)}
    {$IF (CompilerVersion <= 34.0)}
    if UseThemeFont then
      Self.Font.Assign(GetThemeFont);
    {$IFEND}
    {$IF CompilerVersion > 34.0}
    if TIDEThemeMetrics.Font.Enabled then
      Self.Font.Assign(TIDEThemeMetrics.Font.GetFont);
    {$IFEND}

    if ThemeProperties <> nil then
    begin
      LStyle := ThemeProperties.StyleServices;
      StyleElements := StyleElements - [seClient];
      Color := LStyle.GetSystemColor(clWindow);
      BottomPanel.StyleElements := BottomPanel.StyleElements - [seClient];
      BottomPanel.ParentBackground := False;
      BottomPanel.Color := LStyle.GetSystemColor(clBtnFace);
      IDEThemeManager.RegisterFormClass(TStyledButtonEditor);
      ThemeProperties.ApplyTheme(Self);
    end;
  {$IFEND}
{$ENDIF}
end;

procedure TStyledButtonCustomEditor.InitGUI;
var
  I: TStyledButtonDrawType;
  LPos: Integer;
  LDrawName: string;
begin
  Caption := Format(Caption, [StyledComponentsVersion]);
  for I := Low(TStyledButtonDrawType) to High(TStyledButtonDrawType) do
  begin
    LDrawName := GetEnumName(TypeInfo(TStyledButtonDrawType), Ord(I));
    LPos := StyleDrawTypeComboBox.Items.Add(LDrawName);
    if I = SourceButton.StyleDrawType then
      StyleDrawTypeComboBox.ItemIndex := LPos;
  end;
  EnabledCheckBox.Checked := SourceButton.Enabled;
  RadiusTrackBar.Position := SourceButton.StyleRadius;
  FlatButtonCheckBox.Checked := SourceButton.Flat;
  UpdateDestFromGUI;
end;

procedure TStyledButtonCustomEditor.UpdateSizeGUI;
begin
  FUpdating := True;
  try
    Screen.Cursor := crHourGlass;
    InitGUI;
  finally
    FUpdating := False;
    Screen.Cursor := crDefault;
  end;
end;

procedure TStyledButtonCustomEditor.FormShow(Sender: TObject);
begin
  UpdateSizeGUI;

  if SavedBounds.Right - SavedBounds.Left > 0 then
    SetBounds(SavedBounds.Left, SavedBounds.Top, SavedBounds.Width, SavedBounds.Height);
end;

procedure TStyledButtonCustomEditor.HelpButtonClick(Sender: TObject);
begin
  ShellExecute(handle, 'open',
    PChar(GetProjectURL), nil, nil, SW_SHOWNORMAL)
end;

procedure TStyledButtonCustomEditor.Loaded;
begin
  inherited;
end;

procedure TStyledButtonCustomEditor.OKButtonClick(Sender: TObject);
begin
  ApplyStyle;
end;

procedure TStyledButtonCustomEditor.paTopResize(Sender: TObject);
begin
  ActualGroupBox.Width := paTop.Width div 2;
end;

procedure TStyledButtonCustomEditor.RadiusTrackBarChange(Sender: TObject);
begin
  DestButton.StyleRadius := RadiusTrackBar.Position;
  UpdateDestFromGUI;
  //FFamilyBuilt := '';
  //TabControlChange(TabControl);
end;

procedure TStyledButtonCustomEditor.StyleDrawTypeComboBoxSelect(Sender: TObject);
begin
  FCustomStyleDrawType := True;
  UpdateDestFromGUI;
  //FFamilyBuilt := '';
  //TabControlChange(TabControl);
end;

procedure TStyledButtonCustomEditor.UpdateDestFromGUI;
var
  LRoundRect: Boolean;
begin
  DestButton.StyleDrawType := TStyledButtonDrawType(StyleDrawTypeComboBox.ItemIndex);
  LRoundRect := DestButton.StyleDrawType = btRoundRect;
  DestButton.StyleRadius := RadiusTrackBar.Position;
  StyleRadiusLabel.Visible := LRoundRect;
  RadiusTrackBar.Visible := LRoundRect;
  StyleRadiusLabel.Caption := Format('StyleRadius: %d', [DestButton.StyleRadius]);
  DestButton.Enabled := EnabledCheckBox.Checked;
  ActualGroupBox.Caption := Format('ACTUAL: %s/%s/%s',
    [SourceButton.StyleFamily, SourceButton.StyleClass, SourceButton.StyleAppearance]);
  SourceButton.Hint := ActualGroupBox.Caption;
  NewGroupBox.Caption := Format('NEW: %s/%s/%s',
    [DestButton.StyleFamily, DestButton.StyleClass, DestButton.StyleAppearance]);
  DestButton.Hint := NewGroupBox.Caption;
  DestButton.Flat := FlatButtonCheckBox.Checked;
end;

procedure TStyledButtonCustomEditor.FlatButtonCheckBoxClick(Sender: TObject);
begin
  UpdateDestFromGUI;
end;

end.
