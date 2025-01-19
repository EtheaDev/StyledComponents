{******************************************************************************}
{                                                                              }
{  TStyledComponents Demo                                                      }
{  Main Form                                                                   }
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
unit MainDemoForm;

interface

{$INCLUDE StyledComponents.inc}

uses
  System.SysUtils
  , System.Classes
  , System.Types
  , Vcl.Forms
  , Vcl.ActnList
  , Vcl.Controls
  , Vcl.StdCtrls
  , Vcl.ComCtrls
  , Vcl.ButtonStylesAttributes
  , Vcl.StyledButton
  , Vcl.StyledToolbar
  , Vcl.ExtCtrls
  , Vcl.CategoryButtons
  , Vcl.StyledCategoryButtons
  , Vcl.StyledButtonGroup
  , System.Actions
  , uSettings
  {$IFDEF SKIA}
  , Skia.Vcl.StyledTaskDialogAnimatedUnit
  {$ELSE}
  , Vcl.StyledTaskDialogStdUnit
  {$ENDIF}
  ;

const

  MENU_COLLAPSED_WIDTH = 48;
  MENU_EXPANDED_WIDTH = 300;
  SETTINGS_EXPANDED_WIDTH = 400;
  MENU_LINES_HEIGHT = 40;

type
  TfrmMain = class(TForm)
    ActionList: TActionList;
    WorkClientPanel: TPanel;
    WorkPanel: TPanel;
    acQuit: TAction;
    acAbout: TAction;
    actSettings: TAction;
    actMenu: TAction;
    LeftPanel: TPanel;
    catMenuItems: TStyledCategoryButtons;
    panlTop: TPanel;
    lblTitle: TLabel;
    SettingsToolBar: TStyledToolbar;
    ColorSettingsToolButton: TStyledToolButton;
    SepToolButton: TStyledToolButton;
    AboutToolButton: TStyledToolButton;
    QuitToolButton: TStyledToolButton;
    MenuButtonToolbar: TStyledToolbar;
    MenuToolButton: TStyledToolButton;
    acFamilyClassAppearance: TAction;
    WorkTitlePanel: TPanel;
    WorkTitleLabel: TLabel;
    acStyledBitBtn: TAction;
    acAutoClick: TAction;
    acStyledButtonVCLStyles: TAction;
    acControlList: TAction;
    acStyledToolbar: TAction;
    acStyledDbNavigator: TAction;
    acStyledButtonGroup: TAction;
    acStyledCategoryButtons: TAction;
    acStyledTaskDialog: TAction;
    acRoundedCorners: TAction;
    RightPanel: TPanel;
    pc: TPageControl;
    tsFont: TTabSheet;
    FontLabel: TLabel;
    SizeLabel: TLabel;
    CbFont: TComboBox;
    EditFontSize: TEdit;
    FontSizeUpDown: TUpDown;
    PanelTopFont: TPanel;
    stTheme: TTabSheet;
    PanelTopTheme: TPanel;
    stGeneral: TTabSheet;
    PanelTopPreviewSettings: TPanel;
    RoundedButtonsGroupBox: TGroupBox;
    ToolbarRoundedCheckBox: TCheckBox;
    ButtonsRoundedCheckBox: TCheckBox;
    MenuRoundedCheckBox: TCheckBox;
    ThemesRadioGroup: TRadioGroup;
    SelectThemeRadioGroup: TRadioGroup;
    acApplySettings: TAction;
    acCancelSettings: TAction;
    SettingsButtonsPanel: TPanel;
    ApplySettingsButton: TStyledButton;
    CancelSettingsButton: TStyledButton;
    ShowFormGroupBox: TGroupBox;
    cbEmbeddedForms: TCheckBox;
    acAnimatedStyledButton: TAction;
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actSettingsExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actMenuExecute(Sender: TObject);
    procedure acQuitExecute(Sender: TObject);
    procedure acAboutExecute(Sender: TObject);
    procedure acExampleExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CbFontDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure acApplySettingsExecute(Sender: TObject);
    procedure acCancelSettingsExecute(Sender: TObject);
    procedure acSettingsChangedUpdate(Sender: TObject);
    procedure SelectThemeRadioGroupClick(Sender: TObject);
    procedure pcChange(Sender: TObject);
    procedure ThemesRadioGroupClick(Sender: TObject);
    procedure CbFontSelect(Sender: TObject);
    procedure EditFontSizeChange(Sender: TObject);
    procedure ToolbarRoundedCheckBoxClick(Sender: TObject);
    procedure ButtonsRoundedCheckBoxClick(Sender: TObject);
    procedure MenuRoundedCheckBoxClick(Sender: TObject);
  private
    FActiveAction: TAction;
    FActiveView: TForm;
    FFirstTime: Boolean;
    FSettings: TSettings;
    FMenuOpened: Boolean;
    FSettingsOpened: Boolean;
    procedure SetActiveView(const AValue: TFormClass);
  	procedure UpdateFromSettings;
    function GetActiveViewClass: TFormClass;
    procedure SetMenuOpened(const AValue: Boolean);
    function GetScaleFactor: Single;
    procedure ShowDelphiVersion(const AAction: TAction; const ADelphiVer: string);
    procedure AssignImages;
    procedure RealignMainMenu;
    procedure RealignSettingsMenu;
    procedure SetSettingsOpened(const AValue: Boolean);
    function SelectedStyleName: string;
    procedure PopulateAvailThemes;
    procedure UpdateFontSettings;
  protected
    procedure Loaded; override;
    procedure UpdateActions; override;
    procedure UpdateControls;
  public
    destructor Destroy; override;
    property ActiveViewClass: TFormClass read GetActiveViewClass write SetActiveView;
    property MenuOpened: Boolean read FMenuOpened write SetMenuOpened;
    property SettingsOpened: Boolean read FSettingsOpened write SetSettingsOpened;
	procedure ShowError(Sender: TObject; E: Exception);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  System.UITypes
  , Vcl.Graphics
  , Vcl.ImgList
  , Vcl.Themes
  , Winapi.ShlObj
  , DemoWelcomeForm
  , Vcl.Dialogs
  , Vcl.StyledTaskDialog
  , Vcl.StandardButtonStyles
  , Vcl.StyledCmpStrUtils
  {$IFDEF DXE7+}
  , AnimatedButtonsForm
  {$ENDIF}
  {$IFDEF D10_3+}
  , StyledButtonsForm
  , AutoClickForm
  , StyledToolbarForm
  , StyledDbNavigatorForm
  , StyledButtonGroupForm
  , StyledCategoryButtonsForm
  , RoundedCornersForm
  , DResources
  , Vcl.VirtualImageList
  , Vcl.ImageCollection
  {$ELSE}
  , StyledButtonsFormOld
  , AutoClickFormOld
  , StyledToolbarFormOld
  , StyledDbNavigatorFormOld
  , StyledButtonGroupFormOld
  , StyledCategoryButtonsFormOld
  , RoundedCornersFormOld
  , DResourcesOld
  {$ENDIF}
  , StyledButtonVCLStylesForm
  {$IFDEF D11+}
  , ControlListForm
  {$ENDIF}
  , BitBtnForm
  , StyledDialogDemoForm
  , FAboutForm
  ;

{$R *.dfm}

procedure UpdateApplicationStyle(const VCLStyleName: string);
begin
  if StyleServices.Enabled then
    TStyleManager.TrySetStyle(VCLStyleName, False);
end;

{ TfrmMain }

function TfrmMain.GetScaleFactor: Single;
begin
  Result :={$IFDEF D10_3+}Self.ScaleFactor{$ELSE}1{$ENDIF};
end;

procedure TfrmMain.acAboutExecute(Sender: TObject);
begin
  ShowAboutForm;
end;

function TfrmMain.SelectedStyleName: string;
begin
  if SelectThemeRadioGroup.ItemIndex <> -1 then
    Result := SelectThemeRadioGroup.Items[SelectThemeRadioGroup.ItemIndex]
  else
    Result := DefaultStyleName;
end;

procedure TfrmMain.acApplySettingsExecute(Sender: TObject);
begin
  FSettings.WriteSettings;
  UpdateFromSettings;
  RealignMainMenu;
  SettingsOpened := False;
end;

procedure TfrmMain.acCancelSettingsExecute(Sender: TObject);
begin
  FSettings.ReadSettings;
  UpdateFromSettings;
  SettingsOpened := False;
end;

procedure TfrmMain.acExampleExecute(Sender: TObject);
const
  D10_3 = 'from Delphi 10.3';
  D10_4 = 'from Delphi 10.4';
  D11 = 'from Delphi 11';
  D12 = 'from Delphi 12';
  DXE6 = 'from Delphi XE6';

  {$IFDEF DXE7+}
  DXE7_SKIA = 'Using Skia4Delphi. Install it and "Enable Skia" in this demo';
  {$ELSE}
  DXE7_SKIA = 'from Delphi XE7 With Skia enabled';
  {$ENDIF}
begin
  //Some actions can works only with particular Delphi Version to Latest one
  FActiveAction := Sender as TAction;
  if FActiveAction = acFamilyClassAppearance then
    ActiveViewClass := TfmStyledButtons
  else if FActiveAction = acStyledBitBtn then
    ActiveViewClass := TfmBitBtn
  else if FActiveAction = acAutoClick then
    ActiveViewClass := TfmAutoClick
  else if FActiveAction = acStyledButtonVCLStyles then
    ActiveViewClass := TfmStyledButtonVCLStyles
  else if FActiveAction = acControlList then
    {$IFDEF D11+}
    ActiveViewClass := TfmControlList
    {$ELSE}
    ShowDelphiVersion(FActiveAction, D11)
    {$ENDIF}
  else if FActiveAction = acStyledToolbar then
    ActiveViewClass := TfmStyledToolbar
  else if FActiveAction = acStyledDbNavigator then
    ActiveViewClass := TfmStyledDbNavigator
  else if FActiveAction = acStyledButtonGroup then
    ActiveViewClass := TfmStyledButtonGroup
  else if FActiveAction = acStyledCategoryButtons then
    ActiveViewClass := TfmStyledCategoryButtons
  else if FActiveAction = acStyledTaskDialog then
    ActiveViewClass := TfmStyledTaskDialog
  else if FActiveAction = acRoundedCorners then
    ActiveViewClass := TfmRoundedCorners
  else if FActiveAction = acAnimatedStyledButton then
  {$IFDEF SKIA}
    ActiveViewClass := TfmAnimatedButtons
  {$ELSE}
    ShowDelphiVersion(FActiveAction, DXE7_SKIA)
  {$ENDIF}
  else
    ShowDelphiVersion(FActiveAction, DXE6);
  ;
end;

procedure TfrmMain.acQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.acSettingsChangedUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FSettings.IsChanged;
end;

procedure TfrmMain.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  if Screen.Cursor = crHourGlass then
    Screen.Cursor := crDefault;
end;

destructor TfrmMain.Destroy;
begin
  FreeAndNil(FSettings);
  inherited;
end;

procedure TfrmMain.EditFontSizeChange(Sender: TObject);
begin
  FSettings.FontSize := FontSizeUpDown.Position;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
//  WindowState := wsMaximized;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if StyledTaskMessageDlg('Exit from StyledComponents Demo',
    'If you need more info about StyledComponents read the '+
      StringToHRef('https://github.com/EtheaDev/StyledComponents/wiki/','wiki section')+' of the project.'+sLineBreak+
    sLineBreak+
    'Do you want to exit now?',
    mtConfirmation,
    [mbYes, mbNo, mbCancel], 0) <> mrYes then
  Action := caNone;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  LAppName: string;
  LSettingsFileName: TFileName;
begin
  AssignImages;
  //Caption and fonts
  Caption := Application.Title + Format(' - Ver.%s - © 2021-2025 Ethea S.r.l.',
    [StyledComponentsVersion]);
  lblTitle.Caption := Application.Title;
  //Collapse Menu
  MenuOpened := False;
  {$IFNDEF D10_1+}
  catMenuItems.ButtonHeight := Round(MENU_LINES_HEIGHT * GetScaleFactor);
  {$ENDIF}
  //Settings Menu
  SettingsOpened := False;
  LAppName := ChangeFileExt(ExtractFileName(Application.ExeName),'');
  LSettingsFileName := GetSpecialFolder(CSIDL_APPDATA)+LAppName+PathDelim+LAppName+'.ini';
  FSettings := TSettings.CreateSettings(LSettingsFileName);
  CbFont.Items.Assign(Screen.Fonts);
  UpdateFromSettings;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  RealignMainMenu;
  RealignSettingsMenu;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  LeftPanel.Align := alNone;
  RightPanel.Align := alNone;
end;

function TfrmMain.GetActiveViewClass: TFormClass;
begin
  if Assigned(FActiveView) then
    Result := TFormClass(FActiveView.ClassType)
  else
    Result := nil;
end;

procedure TfrmMain.Loaded;
begin
  inherited;
  Font.Assign(Screen.IconFont);
end;

procedure TfrmMain.MenuRoundedCheckBoxClick(Sender: TObject);
begin
  FSettings.MenuDrawRounded := MenuRoundedCheckBox.Checked;
end;

procedure TfrmMain.pcChange(Sender: TObject);
begin
  FSettings.ActivePageIndex := pc.ActivePageIndex;
end;

procedure TfrmMain.SetActiveView(const AValue: TFormClass);
begin
  if Assigned(FActiveView) then
    FreeAndNil(FActiveView);
  FActiveView := AValue.Create(nil);
  if Assigned(FActiveView) then
  begin
    FActiveView.Font.Assign(Self.Font);

    FActiveView.Caption := FActiveAction.Hint;
    WorkTitleLabel.Caption := FActiveView.Caption;
    with FActiveView do
    begin
      if cbEmbeddedForms.Checked then
      begin
        Parent := WorkClientPanel;
        BorderIcons := [];
        BorderStyle := bsNone;
        Align := alClient;
      end
      else
      begin
        Position := poMainFormCenter;
      end;
      Screen.Cursor := crHourGlass;
      try
        //Collapse Menu
        MenuOpened := False;
        if cbEmbeddedForms.Checked then
        begin
          FActiveView.Show;
        end
        else
        begin
          FActiveView.ShowModal;
          FreeAndNil(FActiveView);
        end;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end;
  UpdateControls;
end;

procedure TfrmMain.RealignMainMenu;
var
  LTopHeight: Integer;
  LMenuSize: Integer;
begin
  LTopHeight := panlTop.Height+1;
  if FMenuOpened then
  begin
    LMenuSize := Round(MENU_EXPANDED_WIDTH * GetScaleFactor);
    // When Menu is opened, Shows the Captions of Category Buttons
    catMenuItems.ButtonOptions := catMenuItems.ButtonOptions + [boShowCaptions];
    catMenuItems.ImageAlignment := TImageAlignment.iaLeft;
    actMenu.Hint := 'Collapse Menu';
  end
  else
  begin
    LMenuSize := Round(MENU_COLLAPSED_WIDTH * GetScaleFactor);
    // When Menu is closed, adjust ButtonOptions and Width
    catMenuItems.ButtonOptions := catMenuItems.ButtonOptions - [boShowCaptions];
    catMenuItems.ImageAlignment := TImageAlignment.iaCenter;
    actMenu.Hint := 'Expand Menu';
  end;
  LeftPanel.SetBounds(0, LTopHeight, LMenuSize, ClientHeight-LTopHeight);
  //WorkClientPanel.Margins.Left := LeftPanel.Width;
end;

procedure TfrmMain.RealignSettingsMenu;
var
  LTopHeight: Integer;
  LMenuSize: Integer;
begin
  LTopHeight := panlTop.Height+1;
  if FSettingsOpened then
  begin
    LMenuSize := Round(SETTINGS_EXPANDED_WIDTH * GetScaleFactor);
    actSettings.Hint := 'Collapse Settings';
  end
  else
  begin
    LMenuSize := Round(0);
    WorkClientPanel.Margins.Left := LeftPanel.Width;
    actSettings.Hint := 'Expand Settings';
  end;
  RightPanel.SetBounds(ClientWidth-LMenuSize, LTopHeight, LMenuSize, ClientHeight-LTopHeight);
  //WorkClientPanel.Margins.Right := RightPanel.Width;
end;

procedure TfrmMain.SetMenuOpened(const AValue: Boolean);
begin
  FMenuOpened := AValue;
  if FMenuOpened then
    SettingsOpened := False;
  RealignMainMenu;
end;

procedure TfrmMain.SetSettingsOpened(const AValue: Boolean);
begin
  FSettingsOpened := AValue;
  if FSettingsOpened then
    MenuOpened := False;
  RealignSettingsMenu;
end;

procedure TfrmMain.ShowDelphiVersion(const AAction: TAction; const ADelphiVer: string);
begin
  StyledTaskMessageDlg(AAction.Caption,
    Format('%s%s%sThis example is available only %s!',
      [AAction.Hint, sLineBreak, sLineBreak, ADelphiVer]), mtInformation,
      [mbOK], 0);
end;

procedure TfrmMain.ThemesRadioGroupClick(Sender: TObject);
begin
  PopulateAvailThemes;
  FSettings.ThemeSelection := TThemeSelection(ThemesRadioGroup.ItemIndex);
end;

procedure TfrmMain.ToolbarRoundedCheckBoxClick(Sender: TObject);
begin
  FSettings.ToolbarDrawRounded := ToolbarRoundedCheckBox.Checked;
end;

procedure TfrmMain.UpdateActions;
begin
  inherited;
  if not FFirstTime then
  begin
    FFirstTime := True;
    with TWelcomeForm.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
  end;
end;

procedure TfrmMain.UpdateFontSettings;
begin
  Screen.MessageFont.Name := FSettings.FontName;
  Screen.MessageFont.Size := FSettings.FontSize;

  //Initialize Task Dialogs Defaults, to change Font
  InitializeStyledTaskDialogs(Screen.MessageFont);

  if FSettings.FontSize >= MinfontSize then
    Font.Size := FSettings.FontSize
  else
    Font.Size := MinfontSize;
  if FSettings.FontName <> Font.Name then
    Font.Name := FSettings.FontName;
  if Assigned(FActiveView) then
  begin
    FActiveView.Font.Size := Font.Size;
    FActiveView.Font.Name := Font.Name;
  end;

  lblTitle.Font.Name := Font.Name;
  lblTitle.Font.Size := FSettings.FontSize * 2;

  WorkTitleLabel.Font.Name := Font.Name;
  WorkTitleLabel.Font.Height := Round(-13 * GetScaleFactor);
  WorkTitleLabel.Font.Style := [fsBold];

  PanelTopPreviewSettings.Font.Name := Font.Name;
  PanelTopPreviewSettings.Font.Style := PanelTopPreviewSettings.Font.Style + [fsBold];

  PanelTopTheme.Font.Name := Font.Name;
  PanelTopTheme.Font.Style := PanelTopTheme.Font.Style + [fsBold];

  PanelTopFont.Font.Name := Font.Name;
  PanelTopFont.Font.Style := PanelTopFont.Font.Style + [fsBold];
end;

procedure TfrmMain.UpdateFromSettings;
var
  LStyle: TStyledButtonDrawType;
begin
  FSettings.ReadSettings;
  Screen.Cursor := crHourGlass;
  try
    //Application Style
    UpdateApplicationStyle(FSettings.StyleName);

    //Fonts
    UpdateFontSettings;

    pc.ActivePageIndex := FSettings.ActivePageIndex;
    ThemesRadioGroup.ItemIndex := Ord(FSettings.ThemeSelection);
    CbFont.ItemIndex := CbFont.Items.IndexOf(FSettings.FontName);
    FontSizeUpDown.Position := FSettings.FontSize;
    ToolbarRoundedCheckBox.Checked := FSettings.ToolbarDrawRounded;
    ButtonsRoundedCheckBox.Checked := FSettings.ButtonDrawRounded;
    MenuRoundedCheckBox.Checked := FSettings.MenuDrawRounded;

    //Rounded Buttons for StyledButtons
    if FSettings.ButtonDrawRounded then
      LStyle := btRounded
    else
      LStyle := btRoundRect;
    TStyledButton.RegisterDefaultRenderingStyle(LStyle);

    //Rounded Buttons for StyledToolbars
    if FSettings.ToolbarDrawRounded then
      LStyle := btRounded
    else
      LStyle := btRoundRect;
    TStyledToolbar.RegisterDefaultRenderingStyle(LStyle);
    SettingsToolBar.StyleDrawType := LStyle;
    MenuToolButton.StyleDrawType := LStyle;

    //Rounded Buttons for menus: StyledCategories and StyledButtonGroup
    if FSettings.MenuDrawRounded then
      LStyle := btRounded
    else
      LStyle := btRoundRect;
    TStyledCategoryButtons.RegisterDefaultRenderingStyle(LStyle);
    TStyledButtonGroup.RegisterDefaultRenderingStyle(LStyle);
    catMenuItems.StyleDrawType := LStyle;
    MenuButtonToolbar.StyleDrawType := LStyle;
    PopulateAvailThemes;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.UpdateControls;
//var
//  I: Integer;
begin
(*
  for I := 0 to Pred(SubViewCount) do
    SubViews[I].UpdateControls;
*)
end;

procedure TfrmMain.actMenuExecute(Sender: TObject);
begin
  MenuOpened := not MenuOpened;
end;

procedure TfrmMain.actSettingsExecute(Sender: TObject);
begin
  SettingsOpened := not SettingsOpened;
end;

procedure TfrmMain.AssignImages;
var
  LImageList: TCustomImageList;
  I, J: Integer;
  LCategory: TButtonCategory;
  LItem: TButtonItem;
  {$IFDEF D10_3+}
  LImage: TImageCollectionItem;
  {$ENDIF}
begin
  {$IFDEF D10_3+}
  LImageList := TVirtualImageList.Create(Self);
  TVirtualImageList(LImageList).ImageCollection := dmResources.ImageCollection;
  J := Round(32 * GetScaleFactor);
  TVirtualImageList(LImageList).SetSize(J,J);
  for I := 0 to dmResources.ImageCollection.Count -1 do
  begin
    LImage := dmResources.ImageCollection.Images[I];
    TVirtualImageList(LImageList).Add(LImage.Name, I);
  end;
  {$ELSE}
  LImageList := dmResources.ImageList32;
  {$ENDIF}
  CancelSettingsButton.Images := LImageList;
  ApplySettingsButton.Images := LImageList;
  ActionList.Images := LImageList;
  MenuButtonToolbar.Images := LImageList;
  SettingsToolBar.Images := LImageList;
  catMenuItems.Images := LImageList;
  for I := catMenuItems.Categories.Count-1 downto 0 do
  begin
    LCategory := catMenuItems.Categories[I];
    for J := LCategory.Items.Count-1 downto 0 do
    begin
      LItem := LCategory.Items[J];
      LItem.ImageIndex := (LItem.Action as TAction).ImageIndex;
    end;
  end;
end;

procedure TfrmMain.ButtonsRoundedCheckBoxClick(Sender: TObject);
begin
  FSettings.ButtonDrawRounded := ButtonsRoundedCheckBox.Checked;
end;

procedure TfrmMain.CbFontDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with CbFont do
  begin
    Canvas.fillrect(rect);
    Canvas.Font.Name  := Items[Index];
    Canvas.textout(rect.Left, rect.Top, Items[Index]);
  end;
end;

procedure TfrmMain.CbFontSelect(Sender: TObject);
begin
  FSettings.FontName := CbFont.Text;
end;

procedure TfrmMain.SelectThemeRadioGroupClick(Sender: TObject);
begin
  FSettings.StyleName := SelectedStyleName;
end;

procedure TfrmMain.PopulateAvailThemes;
var
  I: Integer;
  IsLight: Boolean;
  LStyleName: string;
  LThemeAttributes: TThemeAttribute;
begin
  if TThemeSelection(ThemesRadioGroup.ItemIndex) = tsAsWindows then
    IsLight := IsWindowsAppThemeLight
  else
    IsLight := TThemeSelection(ThemesRadioGroup.ItemIndex) = tsLightTheme;

  SelectThemeRadioGroup.Items.Clear;
  for I := 0 to High(TStyleManager.StyleNames) do
  begin
    LStyleName := TStyleManager.StyleNames[I];
    GetStyleAttributes(LStyleName, LThemeAttributes);
    if not Assigned(LThemeAttributes) then
      Continue;
    if IsLight and (LThemeAttributes.ThemeType = ttLight) or
      (not IsLight and (LThemeAttributes.ThemeType = ttDark)) then
      SelectThemeRadioGroup.Items.Add(LStyleName);
  end;
  SelectThemeRadioGroup.OnClick := nil;
  try
    TStringList(SelectThemeRadioGroup.Items).Sort;
{$IFNDEF DISABLE_STYLES}
    SelectThemeRadioGroup.ItemIndex :=
      SelectThemeRadioGroup.Items.IndexOf(TStyleManager.ActiveStyle.Name);
{$ELSE}
    SelectThemeRadioGroup.ItemIndex := 0;
{$ENDIF}
    if SelectThemeRadioGroup.ItemIndex = -1 then
      SelectThemeRadioGroup.ItemIndex := 0;
  finally
    SelectThemeRadioGroup.OnClick := SelectThemeRadioGroupClick;
    SelectThemeRadioGroupClick(SelectThemeRadioGroup);
  end;
end;

procedure TfrmMain.ShowError(Sender: TObject; E: Exception);
var
  Buttons: TMsgDlgButtons;
  Selection: Integer;
  LTitle: string;
  LMessage: string;
  LHelpContext: Integer;
begin
  LTitle := GetErrorClassNameDesc(E.ClassName,
    E is EAccessViolation);
  LMessage := E.Message;
  LHelpContext := 0;

  if E.HelpContext <> 0 then
    LHelpContext := Abs(E.HelpContext);

  Buttons := [mbOK];
  if LHelpContext <> 0 then
    Buttons := Buttons + [mbHelp];
  if E.InheritsFrom(EAccessViolation) then
    Buttons := Buttons + [mbAbort];

  Selection := StyledTaskMessageDlg(LTitle, LMessage, mtError, Buttons, LHelpContext, mbOK);
  if Selection = mrAbort then
    Application.Terminate
  else if Selection = -1 then
    Application.HelpContext(LHelpContext);
end;

initialization
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

end.
