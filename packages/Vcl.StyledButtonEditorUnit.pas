{******************************************************************************}
{                                                                              }
{       StyledButton Editor: Component editor for Styled Button                }
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
unit Vcl.StyledButtonEditorUnit;

interface

{$INCLUDE StyledComponents.inc}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Actions,
  Vcl.ActnList,
  Vcl.StyledButton,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ButtonStylesAttributes,
  Vcl.StandardButtonStyles,
  Vcl.BootstrapButtonStyles,
  Vcl.AngularButtonStyles,
  Vcl.ColorButtonStyles,
  Vcl.ImgList;

const
  BUTTON_WIDTH = 100;
  BUTTON_HEIGHT = 34;
  BUTTON_MARGIN = 10;
type
  TStyledButtonEditor = class(TForm)
    BottomPanel: TPanel;
    OKButton: TButton;
    ApplyButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    paTop: TPanel;
    ActualGroupBox: TGroupBox;
    NewGroupBox: TGroupBox;
    SplitterTop: TSplitter;
    SourceButton: TStyledGraphicButton;
    DestButton: TStyledGraphicButton;
    TabControl: TTabControl;
    AttributesPanel: TPanel;
    AttributesGroupBox: TGroupBox;
    StyleDrawTypeLabel: TLabel;
    StyleRadiusLabel: TLabel;
    EnabledCheckBox: TCheckBox;
    StyleDrawTypeComboBox: TComboBox;
    RadiusTrackBar: TTrackBar;
    ImageList: TImageList;
    ScrollBox: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure paTopResize(Sender: TObject);
    procedure StyleDrawTypeComboBoxSelect(Sender: TObject);
    procedure EnabledCheckBoxClick(Sender: TObject);
    procedure DestButtonClick(Sender: TObject);
    procedure RadiusTrackBarChange(Sender: TObject);
  private
    {$IFNDEF D10_3+}
    FScaleFactor: Single;
    {$ENDIF}
    FUpdating: Boolean;
    FStyledButton: TStyledGraphicButton;
    procedure BuildTabControls;
    procedure BuildFamilyPreview(const AFamily: TStyledButtonFamily);
    procedure BuildButtonsPreview(const AFamily: TStyledButtonFamily;
      const AAppearance: TStyledButtonAppearance; const AFlowPanel: TFlowPanel);
    procedure ApplyStyle;
    procedure InitGUI;
    procedure UpdateDestFromGUI;
    procedure ButtonClick(Sender: TObject);
    procedure UpdateSizeGUI;
    function GetScaleFactor: Single;
    procedure FlowPanelResize(Sender: TObject);
  protected
    procedure Loaded; override;
    {$IFNDEF D10_3+}
    procedure ChangeScale(M, D: Integer); override;
    {$ENDIF}
  public
    {$IFDEF D10_3+}
    procedure ScaleForPPI(NewPPI: Integer); override;
    {$ENDIF}
  end;

function EditStyledButton(const AButton: TStyledGraphicButton): Boolean;

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
  //paTopHeight: Integer;

function EditStyledButton(const AButton: TStyledGraphicButton): Boolean;
var
  LEditor: TStyledButtonEditor;
begin
  LEditor := TStyledButtonEditor.Create(nil);
  with LEditor do
  begin
    try
      FStyledButton := AButton;
      paTop.Height := SourceButton.Top + AButton.Height + 14;
      AButton.AssignStyleTo(SourceButton);
      SourceButton.Enabled := AButton.Enabled;
      SourceButton.Caption := AButton.Caption;
      SourceButton.Hint := AButton.Hint;
      SourceButton.Width := AButton.Width;
      SourceButton.Height := AButton.Height;

      AButton.AssignStyleTo(DestButton);
      DestButton.Enabled := AButton.Enabled;
      DestButton.Width := AButton.Width;
      DestButton.Height := AButton.Height;
      DestButton.Caption := AButton.Caption;
      DestButton.Hint := AButton.Hint;
      Result := ShowModal = mrOk;
      SavedBounds := BoundsRect;
      //paTopHeight := paTop.Height;
    finally
      Free;
    end;
  end;
end;

{ TStyledButtonEditorForm }

procedure TStyledButtonEditor.ButtonClick(Sender: TObject);
var
  LStylesButton: TStyledButton;
begin
  LStylesButton := Sender as TStyledButton;
  LStylesButton.AssignStyleTo(DestButton);
  StyleDrawTypeComboBox.ItemIndex := Ord(LStylesButton.StyleDrawType);
  UpdateDestFromGUI;
end;


procedure TStyledButtonEditor.ApplyStyle;
begin
  Screen.Cursor := crHourglass;
  try
    DestButton.AssignStyleTo(FStyledButton);
    FStyledButton.Enabled := DestButton.Enabled;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TStyledButtonEditor.ApplyButtonClick(Sender: TObject);
begin
  ApplyStyle;
  UpdateDestFromGUI;
end;

procedure TStyledButtonEditor.BuildFamilyPreview(
  const AFamily: TStyledButtonFamily);
var
  I: Integer;
  LAppearances: TButtonAppearances;
  LAppearance: TStyledButtonAppearance;
  LGroupBox: TGroupBox;
  LFlowPanel: TFlowPanel;
  LGroupBoxHeight: Integer;
begin
  //Clear components
  while ScrollBox.ControlCount > 0 do
  begin
    LGroupBox := ScrollBox.Controls[0] as TGroupBox;
    while LGroupBox.ControlCount > 0 do
      LGroupBox.Controls[0].Free;
    LGroupBox.Free;
  end;

  //Build a GroupBox for any Appearance
  LAppearances := GetButtonFamilyAppearances(AFamily);
  LGroupBoxHeight := SourceButton.Top + Round(BUTTON_HEIGHT * GetScaleFactor) + Round(BUTTON_MARGIN * GetScaleFactor);
  for I := 0 to Length(LAppearances)-1 do
  begin
    LAppearance := LAppearances[I];
    LGroupBox := TGroupBox.Create(Self);
    LGroupBox.AlignWithMargins := True;
    LGroupBox.Height := LGroupBoxHeight;
    LGroupBox.Caption := LAppearances[I];
    LGroupBox.Parent := ScrollBox;
    LGroupBox.Top := LGroupBoxHeight * (I);
    LGroupBox.Align := alTop;
    LGroupBox.Caption :=
      Format('StyleFamily: "%s" - StyleAppearance: "%s": List of StyleClass',
        [AFamily, LAppearance]);

    //Create a FlowPanel inside the Groubox
    LFlowPanel := TFlowPanel.Create(Self);
    LFlowPanel.AlignWithMargins := True;
    LFlowPanel.Margins.Top := 6;
    LFlowPanel.Align := alTop;
    LFlowPanel.Height := LGroupBoxHeight;
    LFlowPanel.AutoSize := True;
    LFlowPanel.BevelOuter := bvNone;
    LFlowPanel.OnResize := FlowPanelResize;
    LFlowPanel.Parent := LGroupBox;

    BuildButtonsPreview(AFamily, LAppearance, LFlowPanel);
  end;
end;

procedure TStyledButtonEditor.BuildTabControls;
var
  LFamilies: TObjectList;
  LFamily: TStyledButtonFamily;
  I: Integer;
begin
  TabControl.Tabs.Clear;
  LFamilies := GetButtonFamilies;
  if Assigned(LFamilies) then
  begin
    for I := 0 to LFamilies.Count -1 do
    begin
      LFamily := GetButtonFamilyName(I);
      TabControl.Tabs.Add(LFamily);
      if SameText(LFamily, FStyledButton.StyleFamily) then
        TabControl.TabIndex := I;
    end;
  end;
  TabControlChange(TabControl);
end;

{$IFNDEF D10_3+}
procedure TStyledButtonEditor.ChangeScale(M, D: Integer);
begin
  inherited;
  FScaleFactor := FScaleFactor * M / D;
end;
{$ENDIF}

procedure TStyledButtonEditor.DestButtonClick(Sender: TObject);
begin
  ; //Do nothing
end;

procedure TStyledButtonEditor.EnabledCheckBoxClick(Sender: TObject);
begin
  UpdateDestFromGUI;
end;

procedure TStyledButtonEditor.FormCreate(Sender: TObject);
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

procedure TStyledButtonEditor.InitGUI;
var
  I: TStyledButtonDrawType;
  LPos: Integer;
  LDrawName: string;
begin
  Caption := Format(Caption, [StyledButtonsVersion]);
  BuildTabControls;
  for I := Low(TStyledButtonDrawType) to High(TStyledButtonDrawType) do
  begin
    LDrawName := GetEnumName(TypeInfo(TStyledButtonDrawType), Ord(I));
    LPos := StyleDrawTypeComboBox.Items.Add(LDrawName);
    if I = SourceButton.StyleDrawType then
      StyleDrawTypeComboBox.ItemIndex := LPos;
  end;
  EnabledCheckBox.Checked := SourceButton.Enabled;
  RadiusTrackBar.Position := SourceButton.StyleRadius;
  UpdateDestFromGUI;
end;

procedure TStyledButtonEditor.UpdateSizeGUI;
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

procedure TStyledButtonEditor.FormShow(Sender: TObject);
begin
  UpdateSizeGUI;

  if SavedBounds.Right - SavedBounds.Left > 0 then
    SetBounds(SavedBounds.Left, SavedBounds.Top, SavedBounds.Width, SavedBounds.Height);
end;

function TStyledButtonEditor.GetScaleFactor: Single;
begin
  //ScaleFactor is available only from Delphi 10.3, FScaleFactor is calculated
  {$IFDEF D10_3+}
    Result := ScaleFactor * PixelsPerInch / 96;
  {$ELSE}
    Result := FScaleFactor * PixelsPerInch / 96{$ENDIF};

end;

procedure TStyledButtonEditor.HelpButtonClick(Sender: TObject);
begin
  ShellExecute(handle, 'open',
    PChar(GetProjectURL), nil, nil, SW_SHOWNORMAL)
end;

procedure TStyledButtonEditor.Loaded;
begin
  inherited;
  {$IFNDEF D10_3+}
  FScaleFactor := 1;
  {$ENDIF}
end;

procedure TStyledButtonEditor.OKButtonClick(Sender: TObject);
begin
  ApplyStyle;
end;

procedure TStyledButtonEditor.paTopResize(Sender: TObject);
begin
  ActualGroupBox.Width := paTop.Width div 2;
end;

procedure TStyledButtonEditor.RadiusTrackBarChange(Sender: TObject);
begin
  DestButton.StyleRadius := RadiusTrackBar.Position;
  UpdateDestFromGUI;
end;

procedure TStyledButtonEditor.StyleDrawTypeComboBoxSelect(Sender: TObject);
begin
  UpdateDestFromGUI;
end;

{$IFDEF D10_3+}
procedure TStyledButtonEditor.ScaleForPPI(NewPPI: Integer);
begin
  inherited;
  FScaleFactor := NewPPI / PixelsPerInch;
end;
{$ENDIF}

procedure TStyledButtonEditor.TabControlChange(Sender: TObject);
var
  LFamily: TStyledButtonFamily;
begin
  LFamily := TabControl.Tabs[TabControl.TabIndex];
  BuildFamilyPreview(LFamily);
end;

procedure TStyledButtonEditor.UpdateDestFromGUI;
var
  LRounded: Boolean;
begin
  DestButton.StyleDrawType := TStyledButtonDrawType(StyleDrawTypeComboBox.ItemIndex);
  LRounded := DestButton.StyleDrawType = btRounded;
  DestButton.StyleRadius := RadiusTrackBar.Position;
  StyleRadiusLabel.Visible := LRounded;
  RadiusTrackBar.Visible := LRounded;
  StyleRadiusLabel.Caption := Format('StyleRadius: %d', [DestButton.StyleRadius]);
  DestButton.Enabled := EnabledCheckBox.Checked;
  ActualGroupBox.Caption := Format('ACTUAL: %s/%s/%s',
    [SourceButton.StyleFamily, SourceButton.StyleClass, SourceButton.StyleAppearance]);
  SourceButton.Hint := ActualGroupBox.Caption;
  NewGroupBox.Caption := Format('NEW: %s/%s/%s',
    [DestButton.StyleFamily, DestButton.StyleClass, DestButton.StyleAppearance]);
  DestButton.Hint := NewGroupBox.Caption;
end;

procedure TStyledButtonEditor.BuildButtonsPreview(const AFamily: TStyledButtonFamily;
  const AAppearance: TStyledButtonAppearance;
  const AFlowPanel: TFlowPanel);
var
  J: Integer;
  LClasses: TButtonClasses;
  LDefaultClass: TStyledButtonClass;

  procedure CreateButton(
    const AParent: TFlowPanel;
    const AClass: TStyledButtonClass);
  var
    LStyledButton: TStyledButton;
  begin
    LStyledButton := TStyledButton.Create(Self);
    LStyledButton.Width := Round(BUTTON_WIDTH * GetScaleFactor);
    LStyledButton.Height := Round(BUTTON_HEIGHT * GetScaleFactor);
    LStyledButton.AlignWithMargins := True;
    LStyledButton.Caption := AClass;
    LStyledButton.Hint := Format('StyleFamily: "%s" - StyleAppearance: "%s" - StyleClass: "%s"',
      [AFamily, AAppearance, AClass]);
    LStyledButton.StyleFamily := AFamily;
    LStyledButton.StyleClass := AClass;
    LStyledButton.StyleAppearance := AAppearance;
    LStyledButton.OnClick := ButtonClick;
    LStyledButton.Parent := AParent;
  end;

begin
  if AFlowPanel.ControlCount > 0 then
    Exit;

  Screen.Cursor := crHourGlass;
  Try
    AFlowPanel.OnResize := nil;
    AFlowPanel.DisableAlign;

    LClasses := GetButtonFamilyClasses(AFamily);
    LDefaultClass := LClasses[0];

    //Build Buttons for Family/Class/Appearance
    for J := 0 to Length(LClasses)-1 do
      CreateButton(AFlowPanel, LClasses[J]);
  Finally
    AFlowPanel.OnResize := FlowPanelResize;
    AFlowPanel.EnableAlign;
    Screen.Cursor := crDefault;
  End;
end;

procedure TStyledButtonEditor.FlowPanelResize(Sender: TObject);
begin
  TFlowPanel(Sender).Parent.Height := TFlowPanel(Sender).Height + 20;
end;

end.
