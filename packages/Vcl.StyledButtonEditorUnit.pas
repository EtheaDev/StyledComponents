{******************************************************************************}
{                                                                              }
{  StyledButton Editor: Component editor for Styled Button                     }
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
unit Vcl.StyledButtonEditorUnit;

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
  Vcl.StyledPanel,
  Vcl.ButtonStylesAttributes,
  Vcl.StandardButtonStyles,
  Vcl.BootstrapButtonStyles,
  Vcl.AngularButtonStyles,
  Vcl.ColorButtonStyles,
  Vcl.ImgList;

const
  BUTTON_WIDTH = 100;
  BUTTON_HEIGHT = 34;
  COMPONENT_EDITOR_HELP_URL = 'https://ethea.it/docs/styledcomponents/Component-Editor.html';
  BUTTON_MARGIN = 10;
type
  TStyledButtonEditor = class(TForm)
    BottomPanel: TPanel;
    OKButton: TStyledButton;
    ApplyButton: TStyledButton;
    CancelButton: TStyledButton;
    HelpButton: TStyledButton;
    paTop: TPanel;
    ActualGroupBox: TGroupBox;
    NewGroupBox: TGroupBox;
    SplitterTop: TSplitter;
    TabControl: TTabControl;
    AttributesPanel: TPanel;
    AttributesGroupBox: TGroupBox;
    StyleDrawTypeLabel: TLabel;
    EnabledCheckBox: TCheckBox;
    StyleDrawTypeComboBox: TComboBox;
    ImageList: TImageList;
    ScrollBox: TScrollBox;
    FlatButtonCheckBox: TCheckBox;
    AsVCLComponentCheckBox: TCheckBox;
    StyleLabel: TLabel;
    StyleComboBox: TComboBox;
    RoundedCornersGroupBox: TGroupBox;
    TopRightCheckBox: TCheckBox;
    TopLeftCheckBox: TCheckBox;
    BottomLeftCheckBox: TCheckBox;
    BottomRightCheckBox: TCheckBox;
    CornerRadiusGroupBox: TGroupBox;
    RadiusTrackBar: TTrackBar;
    StyleRadiusLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure paTopResize(Sender: TObject);
    procedure StyleComboBoxSelect(Sender: TObject);
    procedure EnabledCheckBoxClick(Sender: TObject);
    procedure SourceDestControlClick(Sender: TObject);
    procedure RadiusTrackBarChange(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
    procedure AsVCLComponentCheckBoxClick(Sender: TObject);
    procedure RoundedCheckBoxClick(Sender: TObject);
    procedure TabControlGetImageIndex(Sender: TObject; TabIndex: Integer;
      var ImageIndex: Integer);
  private
    FSourceButton: TStyledGraphicButton;
    FDestButton: TStyledGraphicButton;
    FSourcePanel: TStyledPanel;
    FDestPanel: TStyledPanel;
    FUpdating: Boolean;
    FFamilyBuilt: TStyledButtonFamily;
    FStyledButtonRender: TStyledButtonRender;
    FStyledPanel: TStyledPanel;
    FStyledControl: TControl;
    FCustomStyleDrawType: Boolean;
    procedure BuildTabControls;
    procedure ShowFamilyPreview;
    procedure BuildFamilyPreview(const AFamily: TStyledButtonFamily);
    procedure BuildButtonsPreview(const AFamily: TStyledButtonFamily;
      const AAppearance: TStyledButtonAppearance; const AFlowPanel: TFlowPanel);
    procedure ApplyStyle;
    procedure InitGUI;
    procedure UpdateDestFromGUI;
    procedure SelectButtonClick(Sender: TObject);
    procedure SelectPanelClick(Sender: TObject);
    //procedure ButtonEnter(Sender: TObject);
    procedure UpdateSizeGUI;
    procedure FlowPanelResize(Sender: TObject);
    function GetRoundedCorners: TRoundedCorners;
    procedure SetRoundedCorners(const AValue: TRoundedCorners);
  protected
    procedure Loaded; override;
  public
    constructor CreateForSource(const AOwner: TComponent;
      const ASourceControl: TObject);
    property CustomStyleDrawType: Boolean read FCustomStyleDrawType;
    property RoundedCorners: TRoundedCorners read GetRoundedCorners write SetRoundedCorners;
  end;

function EditStyledControl(const AControl: TControl): Boolean; overload;
function EditStyledGraphicButton(const AButton: TCustomStyledGraphicButton): Boolean; overload;
function EditStyledButton(const AButton: TCustomStyledButton): Boolean; overload;
function EditStyledButtonRender(const AButtonRender: TStyledButtonRender): Boolean; overload;
function EditStyledPanel(const APanel: TStyledPanel): Boolean; overload;

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
  SavedWindowState: TWindowState = wsNormal;

procedure SaveEditorBounds(const AEditor: TForm);
begin
  //Bounds of the normal window only: the ones of a maximized window, restored
  //on a normal one, give a window as big as the screen but not maximized
  if AEditor.WindowState = wsNormal then
    SavedBounds := AEditor.BoundsRect;
  if AEditor.WindowState = wsMaximized then
    SavedWindowState := wsMaximized
  else
    SavedWindowState := wsNormal;
end;

function EditStyledControl(const AControl: TControl): Boolean;
begin
  if AControl is TCustomStyledGraphicButton then
    Result := EditStyledGraphicButton(TCustomStyledGraphicButton(AControl))
  else if AControl is TCustomStyledButton then
    Result := EditStyledButton(TCustomStyledButton(AControl))
  else if AControl is TStyledPanel then
    Result := EditStyledPanel(TStyledPanel(AControl))
  else
    raise Exception.CreateFmt('Cannot Edit Control "%s"', [AControl.Name]);
end;

function EditStyledGraphicButton(const AButton: TCustomStyledGraphicButton): Boolean;
begin
  Result := EditStyledButtonRender(AButton.Render);
end;

function EditStyledButton(const AButton: TCustomStyledButton): Boolean;
begin
  Result := EditStyledButtonRender(AButton.Render);
end;

function EditStyledButtonRender(const AButtonRender: TStyledButtonRender): Boolean;
var
  LEditor: TStyledButtonEditor;
begin
  LEditor := TStyledButtonEditor.CreateForSource(nil, AButtonRender);
  with LEditor do
  begin
    try
      FStyledButtonRender := AButtonRender;
      FStyledControl := FStyledButtonRender.OwnerControl;
      paTop.Height := FSourceButton.Top + AButtonRender.Height + FSourceButton.Top;
      AButtonRender.AssignStyleTo(FSourceButton.Render);
      FSourceButton.Enabled := AButtonRender.Enabled;
      FSourceButton.Width := AButtonRender.Width;
      FSourceButton.Height := AButtonRender.Height;
      FSourceButton.Caption := AButtonRender.Caption;
      FSourceButton.Hint := AButtonRender.Hint;

      AButtonRender.AssignStyleTo(FDestButton.Render);
      FDestButton.Enabled := AButtonRender.Enabled;
      FDestButton.Width := AButtonRender.Width;
      FDestButton.Height := AButtonRender.Height;
      FDestButton.Caption := AButtonRender.Caption;
      FDestButton.Hint := AButtonRender.Hint;
      if AButtonRender.StyleDrawType <> DEFAULT_STYLEDRAWTYPE then
        FDestButton.StyleDrawType := AButtonRender.StyleDrawType;

      Result := ShowModal = mrOk;
      SaveEditorBounds(LEditor);
      if Result then
        AButtonRender.OwnerControl.Invalidate;
    finally
      Free;
    end;
  end;
end;

function EditStyledPanel(const APanel: TStyledPanel): Boolean;
var
  LEditor: TStyledButtonEditor;
begin
  LEditor := TStyledButtonEditor.CreateForSource(nil, APanel);
  with LEditor do
  begin
    try
      FStyledPanel := APanel;
      FStyledControl := APanel;
      //paTop.Height := FSourcePanel.Top + APanel.Height + APanel.Top;
      APanel.AssignStyleTo(FSourcePanel);
      FSourcePanel.Enabled := APanel.Enabled;
      FSourcePanel.Width := APanel.Width;
      FSourcePanel.Height := APanel.Height;
      FSourcePanel.Caption := APanel.Caption;
      FSourcePanel.Hint := APanel.Hint;
      if APanel.StyleDrawType <> DEFAULT_STYLEDRAWTYPE then
        FSourcePanel.StyleDrawType := APanel.StyleDrawType;

      APanel.AssignStyleTo(FDestPanel);
      FDestPanel.Enabled := APanel.Enabled;
      FDestPanel.Width := APanel.Width;
      FDestPanel.Height := APanel.Height;
      FDestPanel.Caption := APanel.Caption;
      FDestPanel.Hint := APanel.Hint;
      if APanel.StyleDrawType <> DEFAULT_STYLEDRAWTYPE then
        FDestPanel.StyleDrawType := APanel.StyleDrawType;

      Result := ShowModal = mrOk;
      SaveEditorBounds(LEditor);
      if Result then
        APanel.Invalidate;
    finally
      Free;
    end;
  end;
end;

{ TStyledButtonEditorForm }

procedure TStyledButtonEditor.SelectPanelClick(Sender: TObject);
var
  LPreview: TStyledPanel;
begin
  //Preview panel clicked (editing a TStyledPanel): copy its style to the new panel
  LPreview := TStyledPanel(Sender);
  FCustomStyleDrawType := False;
  FDestPanel.SetCustomStyleDrawType(FCustomStyleDrawType);
  FDestPanel.StyleRadius := LPreview.StyleRadius;
  FDestPanel.StyleRoundedCorners := LPreview.StyleRoundedCorners;
  FDestPanel.StyleDrawType := LPreview.StyleDrawType;
  FDestPanel.StyleFamily := LPreview.StyleFamily;
  FDestPanel.StyleClass := LPreview.StyleClass;
  FDestPanel.StyleAppearance := LPreview.StyleAppearance;
  FDestPanel.CaptionAlignment := LPreview.CaptionAlignment;
  StyleDrawTypeComboBox.ItemIndex := Ord(LPreview.StyleDrawType);
  AsVCLComponentCheckBox.Checked := LPreview.AsVCLComponent;
  UpdateDestFromGUI;
end;

procedure TStyledButtonEditor.SelectButtonClick(Sender: TObject);
var
  LStyledButton: TStyledGraphicButton;
begin
  if Sender is TStyledPanel then
  begin
    SelectPanelClick(Sender);
    Exit;
  end;
  LStyledButton := TStyledGraphicButton(Sender);
  if Assigned(FDestButton) then
  begin
    FCustomStyleDrawType := False;
    LStyledButton.Render.SetCustomStyleDrawType(FCustomStyleDrawType);
    //Assign Style from Button to Button
    LStyledButton.AssignStyleTo(FDestButton.Render);
  end
  else if Assigned(FDestPanel) then
  begin
    FCustomStyleDrawType := False;
    //Assign Style from Button to Panel
    FDestPanel.SetCustomStyleDrawType(FCustomStyleDrawType);
    FDestPanel.StyleRadius := LStyledButton.StyleRadius;
    FDestPanel.StyleRoundedCorners := LStyledButton.StyleRoundedCorners;
    FDestPanel.StyleDrawType := LStyledButton.StyleDrawType;
    FDestPanel.StyleFamily := LStyledButton.StyleFamily;
    FDestPanel.StyleClass := LStyledButton.StyleClass;
    FDestPanel.StyleAppearance := LStyledButton.StyleAppearance;
    FDestPanel.CaptionAlignment := LStyledButton.CaptionAlignment;
  end;
  StyleDrawTypeComboBox.ItemIndex := Ord(LStyledButton.StyleDrawType);
  StyleComboBox.ItemIndex := Ord(LStyledButton.Style);
  AsVCLComponentCheckBox.Checked := LStyledButton.AsVCLComponent;
  UpdateDestFromGUI;
end;

procedure TStyledButtonEditor.ApplyStyle;
begin
  Screen.Cursor := crHourglass;
  try
    if Assigned(FDestButton) then
    begin
      FDestButton.Render.SetCustomStyleDrawType(FCustomStyleDrawType);
      FDestButton.AssignStyleTo(FStyledButtonRender);
      FStyledControl.Enabled := FDestButton.Enabled;
    end
    else if Assigned(FDestPanel) then
    begin
      FDestPanel.AssignStyleTo(FStyledPanel);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TStyledButtonEditor.AsVCLComponentCheckBoxClick(Sender: TObject);
begin
  if AsVCLComponentCheckBox.Checked and not FUpdating then
  begin
    TabControl.TabIndex := 0;
    TabControlChange(TabControl);
  end;
  UpdateDestFromGUI;
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
  LOldCursor: TCursor;
begin
  //One hourglass for the whole page (the previous cursor is restored)
  LOldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
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
  LGroupBoxHeight := BUTTON_HEIGHT + BUTTON_MARGIN;
  for I := 0 to Length(LAppearances)-1 do
  begin
    LAppearance := LAppearances[I];
    LGroupBox := TGroupBox.Create(Self);
    LGroupBox.AlignWithMargins := True;
    LGroupBox.Caption := LAppearances[I];
    LGroupBox.Parent := ScrollBox;
    LGroupBox.Top := LGroupBoxHeight * (I);
    LGroupBox.Align := alTop;
    LGroupBox.Caption :=
      Format('StyleFamily: "%s" - StyleAppearance: "%s": List of StyleClass',
        [AFamily, LAppearance]);

    //Create a FlowPanel inside the GroupBox
    LFlowPanel := TFlowPanel.Create(Self);
    LFlowPanel.AlignWithMargins := True;
    LFlowPanel.Margins.Top := 6;
    LFlowPanel.Margins.Bottom := 6;
    LFlowPanel.Align := alTop;
    LFlowPanel.AutoSize := True;
    LFlowPanel.BevelOuter := bvNone;
    LFlowPanel.OnResize := FlowPanelResize;
    LFlowPanel.DoubleBuffered := True;
    LFlowPanel.Parent := LGroupBox;

    BuildButtonsPreview(AFamily, LAppearance, LFlowPanel);
  end;
  FFamilyBuilt := AFamily;
  finally
    Screen.Cursor := LOldCursor;
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
      if Assigned(FStyledButtonRender) and SameText(LFamily, FStyledButtonRender.StyleFamily) then
        TabControl.TabIndex := I
      else if Assigned(FStyledPanel) and SameText(LFamily, FStyledPanel.StyleFamily) then
        TabControl.TabIndex := I
    end;
  end;
  ShowFamilyPreview;
end;

procedure TStyledButtonEditor.SourceDestControlClick(Sender: TObject);
begin
  ; //Do nothing
end;

procedure TStyledButtonEditor.EnabledCheckBoxClick(Sender: TObject);
begin
  UpdateDestFromGUI;
end;

constructor TStyledButtonEditor.CreateForSource(const AOwner: TComponent;
  const ASourceControl: TObject);
var
  LScaleFactor: Single;
begin
  inherited Create(AOwner);
  LScaleFactor := {$IFDEF D10_3+}ScaleFactor{$ELSE}1{$ENDIF};
  if ASourceControl is TStyledButtonRender then
  begin
    FSourceButton := TStyledGraphicButton.CreateStyled(Self,
      'Windows', 'Windows10', 'Normal');
    FSourceButton.AlignWithMargins := True;
    FSourceButton.Parent := ActualGroupBox;
    FSourceButton.SetBounds(
      Round(3 * LScaleFactor),
      Round(24 * LScaleFactor),
      Round(120 * LScaleFactor),
      Round(34 * LScaleFactor));
    FSourceButton.StyleElements := [seFont, seBorder];
    FSourceButton.Caption := 'Source Button';
    FSourceButton.OnClick := SourceDestControlClick;

    FDestButton := TStyledGraphicButton.CreateStyled(Self,
      'Windows', 'Windows10', 'Normal');
    FDestButton.Parent := NewGroupBox;
    FDestButton.AlignWithMargins := True;
    FDestButton.SetBounds(
      Round(12 * LScaleFactor),
      Round(24 * LScaleFactor),
      Round(120 * LScaleFactor),
      Round(34 * LScaleFactor));
    FDestButton.StyleElements := [seFont, seBorder];
    FDestButton.Caption := 'Dest Button';
    FDestButton.OnClick := SourceDestControlClick;
  end
  else if ASourceControl is TStyledPanel then
  begin
    FSourcePanel := TStyledPanel.CreateStyled(Self,
      'Windows', 'Windows10', 'Normal');
    FSourcePanel.AlignWithMargins := True;
    FSourcePanel.Parent := ActualGroupBox;
    FSourcePanel.Align := alClient;
    FSourcePanel.StyleElements := [seFont, seBorder];
    FSourcePanel.Caption := 'Source Panel';

    FDestPanel := TStyledPanel.CreateStyled(Self,
      'Windows', 'Windows10', 'Normal');
    FDestPanel.Parent := NewGroupBox;
    FDestPanel.Align := alClient;
    FDestPanel.AlignWithMargins := True;
    FDestPanel.StyleElements := [seFont, seBorder];
    FDestPanel.Caption := 'Dest Panel';
  end;
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

      OKButton.StyleClass := LStyle.Name;
      CancelButton.StyleClass := LStyle.Name;
      ApplyButton.StyleClass := LStyle.Name;
      HelpButton.StyleClass := LStyle.Name;

      IDEThemeManager.RegisterFormClass(TStyledButtonEditor);
      ThemeProperties.ApplyTheme(Self);
    end;
  {$IFEND}
{$ENDIF}
end;

procedure TStyledButtonEditor.InitGUI;
var
  I: TStyledButtonDrawType;
  J: TCustomButton.TButtonStyle;
  LPos: Integer;
  LDrawName, LStyleName: string;
begin
  TabControl.OnChange := nil;
  try
    if Assigned(FSourcePanel) then
      Caption := Format(Caption, ['Panel', StyledComponentsVersion])
    else
      Caption := Format(Caption, ['Button', StyledComponentsVersion]);
    for I := Low(TStyledButtonDrawType) to High(TStyledButtonDrawType) do
    begin
      LDrawName := GetEnumName(TypeInfo(TStyledButtonDrawType), Ord(I));
      LPos := StyleDrawTypeComboBox.Items.Add(LDrawName);
      if Assigned(FSourceButton) and (I = FSourceButton.StyleDrawType) then
        StyleDrawTypeComboBox.ItemIndex := LPos;
      if Assigned(FSourcePanel) and (I = FSourcePanel.StyleDrawType) then
        StyleDrawTypeComboBox.ItemIndex := LPos;
    end;

    if Assigned(FSourceButton) then
      AsVCLComponentCheckBox.Checked := FSourceButton.AsVCLComponent
    else if Assigned(FSourcePanel) then
      AsVCLComponentCheckBox.Checked := FSourcePanel.AsVCLComponent;

    for J := Low(TCustomButton.TButtonStyle) to High(TCustomButton.TButtonStyle) do
    begin
      LStyleName := GetEnumName(TypeInfo(TCustomButton.TButtonStyle), Ord(J));
      LPos := StyleComboBox.Items.Add(LStyleName);
      if Assigned(FSourceButton) and (J = FSourceButton.Style) then
        StyleComboBox.ItemIndex := LPos;
      if Assigned(FSourcePanel) then
        StyleComboBox.Visible := False;
    end;

    if Assigned(FSourceButton) then
    begin
      EnabledCheckBox.Checked := FSourceButton.Enabled;
      AsVCLComponentCheckBox.Checked := FSourceButton.AsVCLComponent;
      RadiusTrackBar.Position := FSourceButton.StyleRadius;
      RoundedCorners := FSourceButton.StyleRoundedCorners;
      FlatButtonCheckBox.Checked := FSourceButton.Flat;
    end
    else if Assigned(FSourcePanel) then
    begin
      EnabledCheckBox.Checked := FSourcePanel.Enabled;
      AsVCLComponentCheckBox.Checked := FSourcePanel.AsVCLComponent;
      RadiusTrackBar.Position := FSourcePanel.StyleRadius;
      RoundedCorners := FSourcePanel.StyleRoundedCorners;
      FlatButtonCheckBox.Visible := False;
    end;


    UpdateDestFromGUI;
  finally
    TabControl.OnChange := TabControlChange;
  end;
  //Initialisation done: build only the page of the style of the component
  FUpdating := False;
  BuildTabControls;
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
  //After the normal bounds, so that "Restore" goes back to them
  if SavedWindowState = wsMaximized then
    WindowState := wsMaximized;
end;

function TStyledButtonEditor.GetRoundedCorners: TRoundedCorners;
begin
  Result := [];
  if TopLeftCheckBox.Checked then
    Result := Result + [TRoundedCorner.rcTopLeft];
  if TopRightCheckBox.Checked then
    Result := Result + [TRoundedCorner.rcTopRight];
  if BottomRightCheckBox.Checked then
    Result := Result + [TRoundedCorner.rcBottomRight];
  if BottomLeftCheckBox.Checked then
    Result := Result + [TRoundedCorner.rcBottomLeft];
end;

procedure TStyledButtonEditor.SetRoundedCorners(const AValue: TRoundedCorners);
begin
  TopLeftCheckBox.Checked := TRoundedCorner.rcTopLeft in AValue;
  TopRightCheckBox.Checked := TRoundedCorner.rcTopRight in AValue;
  BottomRightCheckBox.Checked := TRoundedCorner.rcBottomRight in AValue;
  BottomLeftCheckBox.Checked := TRoundedCorner.rcBottomLeft in AValue;
end;

procedure TStyledButtonEditor.HelpButtonClick(Sender: TObject);
begin
  //Page of the documentation dedicated to this editor
  ShellExecute(Handle, 'open',
    PChar(COMPONENT_EDITOR_HELP_URL), nil, nil, SW_SHOWNORMAL);
end;

procedure TStyledButtonEditor.Loaded;
begin
  inherited;
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
  if Assigned(FDestButton) then
  begin
    FDestButton.StyleRadius := RadiusTrackBar.Position;
    FDestButton.StyleRoundedCorners := RoundedCorners;
  end
  else if Assigned(FDestPanel) then
  begin
    FDestPanel.StyleRadius := RadiusTrackBar.Position;
    FDestPanel.StyleRoundedCorners := RoundedCorners;
  end;
  UpdateDestFromGUI;
  FFamilyBuilt := '';
  TabControlChange(TabControl);
end;

procedure TStyledButtonEditor.RoundedCheckBoxClick(Sender: TObject);
begin
  UpdateDestFromGUI;
  FFamilyBuilt := '';
  TabControlChange(TabControl);
end;

procedure TStyledButtonEditor.StyleComboBoxSelect(Sender: TObject);
begin
  FCustomStyleDrawType := True;
  UpdateDestFromGUI;
  FFamilyBuilt := '';
  TabControlChange(TabControl);
end;

procedure TStyledButtonEditor.ShowFamilyPreview;
var
  LFamily: TStyledButtonFamily;
begin
  if (TabControl.TabIndex < 0) or (TabControl.TabIndex >= TabControl.Tabs.Count) then
    Exit;
  LFamily := TabControl.Tabs[TabControl.TabIndex];
  if FFamilyBuilt <> LFamily then
    BuildFamilyPreview(LFamily);
end;

procedure TStyledButtonEditor.TabControlChange(Sender: TObject);
begin
  //While the controls are being initialised (InitGUI sets checkboxes, trackbar
  //and radius, each firing a handler that rebuilds the page) nothing is built:
  //BuildTabControls shows the page of the style of the component once, at the end
  if not FUpdating then
    ShowFamilyPreview;
end;

procedure TStyledButtonEditor.TabControlGetImageIndex(Sender: TObject;
  TabIndex: Integer; var ImageIndex: Integer);
begin
  //Use always Image n.6 for custom Families
  if TabIndex <= 5 then
    ImageIndex := TabIndex
  else
    ImageIndex := 6;
end;

procedure TStyledButtonEditor.UpdateDestFromGUI;
begin
  if Assigned(FSourceButton) then
  begin
    FDestButton.Style := TCustomButton.TButtonStyle(StyleComboBox.ItemIndex);
    FDestButton.StyleDrawType := TStyledButtonDrawType(StyleDrawTypeComboBox.ItemIndex);
    FDestButton.StyleRadius := RadiusTrackBar.Position;
    CornerRadiusGroupBox.Visible := FDestButton.StyleDrawType = btRoundRect;
    RoundedCornersGroupBox.Visible := FDestButton.StyleDrawType in [btRoundRect, btRounded];
    FDestButton.StyleRoundedCorners := RoundedCorners;
    FDestButton.Enabled := EnabledCheckBox.Checked;
    FDestButton.Hint := NewGroupBox.Caption;
    FSourceButton.Hint := ActualGroupBox.Caption;
    FDestButton.Flat := FlatButtonCheckBox.Checked;
    FDestButton.AsVCLComponent := AsVCLComponentCheckBox.Checked;
    StyleRadiusLabel.Caption := Format('StyleRadius: %d', [FDestButton.StyleRadius]);
    ActualGroupBox.Caption := Format('ACTUAL: %s/%s/%s',
      [FSourceButton.StyleFamily, FSourceButton.StyleClass, FSourceButton.StyleAppearance]);
    NewGroupBox.Caption := Format('NEW: %s/%s/%s',
      [FDestButton.StyleFamily, FDestButton.StyleClass, FDestButton.StyleAppearance]);
  end
  else if Assigned(FSourcePanel) then
  begin
    FDestPanel.StyleDrawType := TStyledButtonDrawType(StyleDrawTypeComboBox.ItemIndex);
    FDestPanel.StyleRadius := RadiusTrackBar.Position;
    CornerRadiusGroupBox.Visible := FDestPanel.StyleDrawType = btRoundRect;
    RoundedCornersGroupBox.Visible := FDestPanel.StyleDrawType in [btRoundRect, btRounded];
    FDestPanel.StyleRoundedCorners := RoundedCorners;
    FDestPanel.Enabled := EnabledCheckBox.Checked;
    FDestPanel.Hint := NewGroupBox.Caption;
    FSourcePanel.Hint := ActualGroupBox.Caption;
    //FDestPanel.Flat := FlatButtonCheckBox.Checked;
    FDestPanel.AsVCLComponent := AsVCLComponentCheckBox.Checked;
    StyleRadiusLabel.Caption := Format('StyleRadius: %d', [FDestPanel.StyleRadius]);
    ActualGroupBox.Caption := Format('ACTUAL: %s/%s/%s',
      [FSourcePanel.StyleFamily, FSourcePanel.StyleClass, FSourcePanel.StyleAppearance]);
    NewGroupBox.Caption := Format('NEW: %s/%s/%s',
      [FDestPanel.StyleFamily, FDestPanel.StyleClass, FDestPanel.StyleAppearance]);
  end;
end;

procedure TStyledButtonEditor.BuildButtonsPreview(
  const AFamily: TStyledButtonFamily;
  const AAppearance: TStyledButtonAppearance;
  const AFlowPanel: TFlowPanel);
var
  J: Integer;
  LClasses: TButtonClasses;
  LDefaultClass: TStyledButtonClass;

  function NormalizedName(const AName: string): string;
  begin
    Result := StringReplace(AName,' ','_',[rfReplaceAll]);
    Result := StringReplace(Result,'-','_',[rfReplaceAll]);
  end;

  procedure CreatePanel(
    const AParent: TFlowPanel;
    const AClass: TStyledButtonClass);
  var
    LPanel: TStyledPanel;
  begin
    //Editing a TStyledPanel: the preview items are panels, with the colours a
    //panel has in that style (not the ones of the button of the same class)
    LPanel := TStyledPanel.CreateStyled(Self, AFamily, AClass, AAppearance);
    LPanel.Width := BUTTON_WIDTH;
    LPanel.Height := BUTTON_HEIGHT;
    LPanel.AlignWithMargins := True;
    LPanel.Caption := AClass;
    LPanel.Hint := Format('StyleFamily: "%s" - StyleClass: "%s" - StyleAppearance: "%s"',
      [AFamily, AClass, AAppearance]);
    LPanel.ShowHint := True;
    //Forced hand cursor: the preview panel is clickable to choose its style
    LPanel.Cursor := crHandPoint;
    LPanel.OnClick := SelectButtonClick;
    LPanel.StyleDrawType := TStyledButtonDrawType(StyleDrawTypeComboBox.ItemIndex);
    LPanel.StyleRadius := RadiusTrackBar.Position;
    LPanel.StyleRoundedCorners := RoundedCorners;
    LPanel.ParentBackground := False;
    LPanel.Parent := AParent;
  end;

  procedure CreateButton(
    const AParent: TFlowPanel;
    const AClass: TStyledButtonClass);
  var
    LStyledButton: TStyledGraphicButton;
  begin
    LStyledButton := TStyledGraphicButton.Create(Self);
    LStyledButton.Width := BUTTON_WIDTH;
    LStyledButton.Height := BUTTON_HEIGHT;
    LStyledButton.AlignWithMargins := True;
    LStyledButton.Caption := AClass;
    LStyledButton.Hint := Format('StyleFamily: "%s" - StyleClass: "%s" - StyleAppearance: "%s"',
      [AFamily, AClass, AAppearance]);
    LStyledButton.StyleFamily := AFamily;
    LStyledButton.StyleClass := AClass;
    LStyledButton.StyleAppearance := AAppearance;
    LStyledButton.OnClick := SelectButtonClick;
    LStyledButton.Style := TCustomButton.TButtonStyle(StyleComboBox.ItemIndex);
    LStyledButton.StyleDrawType := TStyledButtonDrawType(StyleDrawTypeComboBox.ItemIndex);
    LStyledButton.StyleRadius := RadiusTrackBar.Position;
    LStyledButton.StyleRoundedCorners := RoundedCorners;
    //Only the Classic/Windows preview follows the active VCL style (AsVCL): choosing
    //it must give back a button AsVCL, like the Windows preview of the panels
    LStyledButton.AsVCLComponent :=
      (AFamily = DEFAULT_CLASSIC_FAMILY) and (AClass = DEFAULT_WINDOWS_CLASS);
    LStyledButton.Parent := AParent;
  end;

begin
  if AFlowPanel.ControlCount > 0 then
    Exit;

  Try
    AFlowPanel.OnResize := nil;
    AFlowPanel.DisableAlign;

    LClasses := GetButtonFamilyClasses(AFamily);
    LDefaultClass := LClasses[0];

    //Build Buttons or Panels for Family/Class/Appearance
    for J := 0 to Length(LClasses)-1 do
      if Assigned(FSourcePanel) then
        CreatePanel(AFlowPanel, LClasses[J])
      else
        CreateButton(AFlowPanel, LClasses[J])
  Finally
    AFlowPanel.OnResize := FlowPanelResize;
    AFlowPanel.EnableAlign;
  End;
end;

procedure TStyledButtonEditor.CheckBoxClick(Sender: TObject);
begin
  UpdateDestFromGUI;
end;

procedure TStyledButtonEditor.FlowPanelResize(Sender: TObject);
begin
  if Assigned(FSourceButton) then
  begin
    TFlowPanel(Sender).Parent.Height := TFlowPanel(Sender).Height +
      FSourceButton.Top;
  end
  else if Assigned(FSourcePanel) then
  begin
    TFlowPanel(Sender).Parent.Height := TFlowPanel(Sender).Height +
      FSourcePanel.Top;
  end;
end;

end.
