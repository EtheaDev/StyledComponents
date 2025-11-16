{******************************************************************************}
{                                                                              }
{  StyledComponents Library                                                    }
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
unit StyledButtonVCLStylesFormOld;

interface

{$I StyledComponents.inc}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  {$IFDEF D10_3+}
  System.ImageList,
  Vcl.VirtualImageList,
  Vcl.ImageCollection,
  DResources,
  {$ELSE}
  DResourcesOld,
  {$ENDIF}
  Vcl.ImgList,
  Vcl.StyledButton,
  Vcl.StyledPanel,
  Vcl.ButtonStylesAttributes,
  Vcl.ActnList,
  Vcl.StyledButtonEditorUnit,
  Vcl.Menus,
  Vcl.ComCtrls;

const
  BUTTON_HEIGHT = 28;
  BUTTON_WIDTH = 150;
  BUTTON_MARGIN = 4;
  PANEL_HEIGHT = 28;
  PANEL_WIDTH = 150;
  PANEL_MARGIN = 4;
  VERT_SCROLL_WIDTH = 20;
  COMMANDLINK_HINT = 'CommandLink Hint very long.';

  RENDER_SAME_AS_VCL = 0;
  RENDER_ROUNDED = 1;
  RENDER_ROUNDRECT = 2;
  RENDER_RECTANGLE = 3;
  RENDER_FAB = 4;

  STYLE_PUSHBUTTON = 0;
  STYLE_SPLITBUTTON = 1;
  STYLE_COMMANDLINK = 2;

type
  TfmStyledButtonVCLStyles = class(TForm)
    PopupMenu: TPopupMenu;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Exit1: TMenuItem;
    MainPageControl: TPageControl;
    tsStyledButtons: TTabSheet;
    tsStyledPanels: TTabSheet;
    TopPanel: TPanel;
    RenderRadioGroup: TRadioGroup;
    StyleRadioGroup: TRadioGroup;
    GroupBox1: TGroupBox;
    EnabledCheckBox: TCheckBox;
    OutlineCheckBox: TCheckBox;
    LeftPanel: TPanel;
    LeftScrollBox: TScrollBox;
    Panel1: TPanel;
    RightPanel: TPanel;
    RightScrollBox: TScrollBox;
    TopRightPanel: TPanel;
    PanelTopPanel: TPanel;
    PanelRenderRadioGroup: TRadioGroup;
    PanelAttributesGroup: TGroupBox;
    PanelShowCaptionCheckBox: TCheckBox;
    PanelOutlineCheckBox: TCheckBox;
    PanelLeftPanel: TPanel;
    PanelLeftScrollBox: TScrollBox;
    PanelLeftTopPanel: TPanel;
    PanelRightPanel: TPanel;
    PanelRightScrollBox: TScrollBox;
    PanelRightTopPanel: TPanel;
    ImageList32: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure RadioGroupClick(Sender: TObject);
    procedure PopUpMenuClick(Sender: TObject);
    procedure SplitButtonsCheckBoxClick(Sender: TObject);
    procedure EnabledCheckBoxClick(Sender: TObject);
  private
    FStyleNames: TStringList;
    FImageList: TCustomImageList;
    FButtonsInColumn: Integer;
    FPanelsInColumn: Integer;
    procedure CreateAllButtons(const AForce: Boolean = True);
    procedure CreateAllPanels(const AForce: Boolean = True);
    function GetScaleFactor: Single;
    procedure GetButtonSize(out AWidth, AHeight: Integer);
    procedure GetPanelSize(out AWidth, AHeight: Integer);
    procedure BuildStyleList;
    procedure CreateButtons(const AVcl: Boolean; const AButtonsInColumn: Integer);
    procedure CreatePanels(const AVcl: Boolean; const APanelsInColumn: Integer);
    procedure ClearButtons(AScrollBox: TScrollBox);
    procedure ClearPanels(AScrollBox: TScrollBox);
    procedure ButtonClick(Sender: TObject);
    procedure PanelClick(Sender: TObject);
  protected
  end;

var
  fmStyledButtonVCLStyles: TfmStyledButtonVCLStyles;

implementation

{$R *.dfm}

uses
  System.TypInfo
  , Vcl.StandardButtonStyles
  , Vcl.StyledTaskDialog
  , Vcl.Themes
  , WinApi.ShellAPI
  , FAboutForm
  ;

{ TMainForm }

procedure TfmStyledButtonVCLStyles.ClearButtons(AScrollBox: TScrollBox);
begin
  //Clear previous Buttons
  AScrollBox.Visible := False;
  try
    while AScrollBox.ControlCount > 0 do
      AScrollBox.Controls[AScrollBox.ControlCount-1].Free;
  finally
    AScrollBox.Visible := True;
  end;
end;

procedure TfmStyledButtonVCLStyles.CreateAllButtons(const AForce: Boolean = True);
var
  LButtonsInColumn: Integer;
  LContainerWidth, LWidth, LHeight: Integer;
begin
  if AForce then
    FButtonsInColumn := 0;
  GetButtonSize(LWidth, LHeight);
  LContainerWidth := LeftPanel.Width - Round(VERT_SCROLL_WIDTH * GetScaleFactor);
  LButtonsInColumn := Trunc(LContainerWidth / GetScaleFactor) div (LWidth+BUTTON_MARGIN);
  if LButtonsInColumn <> FButtonsInColumn then
  begin
    try
      CreateButtons(True, LButtonsInColumn);
      CreateButtons(False, LButtonsInColumn);
    finally
      FButtonsInColumn := LButtonsInColumn;
    end;
  end;
end;

procedure TfmStyledButtonVCLStyles.CreateButtons(const AVcl: Boolean;
  const AButtonsInColumn: Integer);
var
  I, LRow: integer;
  LStyleName: string;
  LColumn: Integer;
  LWidth, LHeight: Integer;
  LButton: TButton;

  procedure CreateVCLButton(AColumn, ATop: Integer;
    AStyleName: string);
  begin
    LButton := TButton.Create(Self);
    LButton.Enabled := EnabledCheckBox.Checked;
    LButton.SetBounds((AColumn * LWidth) + (BUTTON_MARGIN*AColumn),
      ATop, LWidth, LHeight);
    case StyleRadioGroup.ItemIndex of
      STYLE_PUSHBUTTON: LButton.Style := bsPushButton;
      STYLE_SPLITBUTTON: LButton.Style := bsSplitButton;
      STYLE_COMMANDLINK: LButton.Style := bsCommandLink;
    end;
    if LButton.Style = bsSplitButton then
      LButton.DropDownMenu := Self.PopupMenu;
    {$IFDEF D10_4+}
    LButton.StyleName := AStyleName;
    {$ENDIF}
    LButton.CommandLinkHint := COMMANDLINK_HINT;
    LButton.Caption := AStyleName;
    LButton.Hint := AStyleName;
    LButton.Parent := LeftScrollBox;
    LButton.PopupMenu := Self.PopupMenu;
    LButton.OnClick := ButtonClick;
  end;

  procedure CreateStyledButton(AColumn, ATop: Integer;
    AStyleName: string);
  var
    LButton: TStyledButton;
    LAsVcl: Boolean;
  begin
    LAsVcl := RenderRadioGroup.ItemIndex = RENDER_SAME_AS_VCL;
    LButton := TStyledButton.CreateStyled(Self,
      DEFAULT_CLASSIC_FAMILY,
      AStyleName,
      DEFAULT_APPEARANCE,
      DEFAULT_STYLEDRAWTYPE,
      DEFAULT_CURSOR,
      not LAsVcl);

    LButton.Enabled := EnabledCheckBox.Checked;
    LButton.SetBounds((AColumn * LWidth) + (BUTTON_MARGIN*AColumn),
      ATop, LWidth, LHeight);
    if RenderRadioGroup.ItemIndex <> RENDER_FAB then
    begin
      case RenderRadioGroup.ItemIndex of
        RENDER_ROUNDED: LButton.StyleDrawType := btRounded; //All buttons Rounded
        RENDER_ROUNDRECT: LButton.StyleDrawType := btRoundRect; //All buttons RoundRect
        RENDER_RECTANGLE: LButton.StyleDrawType := btRect; //All buttons Rect
      end;
      LButton.CommandLinkHint := COMMANDLINK_HINT;
      LButton.Caption := AStyleName;
      case StyleRadioGroup.ItemIndex of
        STYLE_PUSHBUTTON: LButton.Style := bsPushButton;
        STYLE_SPLITBUTTON: LButton.Style := bsSplitButton;
        STYLE_COMMANDLINK: LButton.Style := bsCommandLink;
      end;
      if LButton.Style = bsSplitButton then
        LButton.DropDownMenu := Self.PopupMenu
      else
        LButton.DropDownMenu := nil;
      if OutlineCheckBox.Checked then
        LButton.StyleAppearance := 'Outline'
      else
        LButton.StyleAppearance := 'Normal';
    end
    else
    begin
      //Render FAB button
      LButton.StyleDrawType := btEllipse;
      LButton.Images := FImageList;
      LButton.ImageAlignment := iaCenter;
      LButton.ImageIndex := I mod FImageList.Count;
    end;
    LButton.Hint := AStyleName;
    LButton.Parent := RightScrollBox;
    LButton.PopupMenu := Self.PopupMenu;
    LButton.OnClick := ButtonClick;
  end;

begin
  GetButtonSize(LWidth, LHeight);
  Screen.Cursor := crHourGlass;
  try
    //Clear previous Buttons
    if AVcl then
      ClearButtons(LeftScrollBox)
    else
      ClearButtons(RightScrollBox);
    //Create VCL Buttons
    for I := 0 to FStyleNames.Count -1 do
    begin
      LStyleName := FStyleNames.Strings[I];
      LColumn := (I mod AButtonsInColumn);
      LRow := (I div AButtonsInColumn);
      if AVcl then
        CreateVCLButton(LColumn, LRow*(LHeight+BUTTON_MARGIN), LStyleName)
      else
        CreateStyledButton(LColumn, LRow*(LHeight+BUTTON_MARGIN), LStyleName)
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmStyledButtonVCLStyles.EnabledCheckBoxClick(Sender: TObject);
begin
  if MainPageControl.ActivePage = tsStyledButtons then
    CreateAllButtons
  else if MainPageControl.ActivePage = tsStyledPanels then
    CreateAllPanels;
end;

procedure TfmStyledButtonVCLStyles.FormCreate(Sender: TObject);
{$IFDEF D10_3+}
var
  I: Integer;
  J: Integer;
  LImage: TImageCollectionItem;
{$ENDIF}
begin
  //Caption := Application.Title + ' - ' + Caption;
  BuildStyleList;
  CreateAllButtons;
  CreateAllPanels;
  {$IFDEF D10_3+}
  FImageList := TVirtualImageList.Create(Self);
  TVirtualImageList(FImageList).ImageCollection := dmResources.ImageCollection;
  J := Round(32 * GetScaleFactor);
  TVirtualImageList(FImageList).SetSize(J,J);
  for I := 0 to dmResources.ImageCollection.Count -1 do
  begin
    LImage := dmResources.ImageCollection.Images[I];
    TVirtualImageList(FImageList).Add(LImage.Name, I);
  end;
  {$ELSE}
  FImageList := dmResources.ImageList32;
  {$ENDIF}
end;

procedure TfmStyledButtonVCLStyles.FormDestroy(Sender: TObject);
begin
  FStyleNames.Free;
end;

procedure TfmStyledButtonVCLStyles.FormResize(Sender: TObject);
begin
  if MainPageControl.ActivePage = tsStyledButtons then
  begin
    if LeftPanel.Width <> ClientWidth div 2 then
    begin
      LeftPanel.Width := ClientWidth div 2;
      CreateAllButtons(False);
    end;
  end
  else if MainPageControl.ActivePage = tsStyledPanels then
  begin
    if PanelLeftPanel.Width <> ClientWidth div 2 then
    begin
      PanelLeftPanel.Width := ClientWidth div 2;
      CreateAllPanels(False);
    end;
  end;
end;

procedure TfmStyledButtonVCLStyles.GetButtonSize(out AWidth, AHeight: Integer);
begin
  if RenderRadioGroup.ItemIndex <> RENDER_FAB then
  begin
    AWidth := BUTTON_WIDTH;
    AHeight := BUTTON_HEIGHT;
  end
  else
  begin
    AWidth := BUTTON_HEIGHT * 2;
    AHeight := BUTTON_HEIGHT * 2;
  end;
  if StyleRadioGroup.ItemIndex = STYLE_COMMANDLINK then
  begin
    Inc(AHeight, 40);
    Inc(AWidth, 40);
  end;
end;

function TfmStyledButtonVCLStyles.GetScaleFactor: Single;
begin
  Result :={$IFDEF D10_3+}Self.ScaleFactor{$ELSE}1{$ENDIF};
end;

procedure TfmStyledButtonVCLStyles.PopUpMenuClick(Sender: TObject);
begin
  StyledShowMessage((Sender as TMenuItem).Caption);
end;

procedure TfmStyledButtonVCLStyles.RadioGroupClick(Sender: TObject);
begin
  if MainPageControl.ActivePage = tsStyledButtons then
    CreateAllButtons
  else if MainPageControl.ActivePage = tsStyledPanels then
    CreateAllPanels;
end;

procedure TfmStyledButtonVCLStyles.ScrollBoxMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
Var
  msg: Cardinal;
  code: Cardinal;
  i, n: Integer;
begin
  Handled := True;
  If ssShift In Shift Then
    msg := WM_HSCROLL
  Else
    msg := WM_VSCROLL;

  If WheelDelta > 0 Then
    code := SB_LINEUP
  Else
    code := SB_LINEDOWN;

  n := Mouse.WheelScrollLines * 4; //Speed Up scrolling
  For i:= 1 to n Do
  begin
    if MainPageControl.ActivePage = tsStyledButtons then
    begin
      LeftScrollBox.Perform( msg, code, 0 );
      RightScrollBox.Perform( msg, code, 0 );
    end
    else if MainPageControl.ActivePage = tsStyledPanels then
    begin
      PanelLeftScrollBox.Perform( msg, code, 0 );
      PanelRightScrollBox.Perform( msg, code, 0 );
    end;
  end;
  if MainPageControl.ActivePage = tsStyledButtons then
  begin
    LeftScrollBox.Perform( msg, SB_ENDSCROLL, 0 );
    RightScrollBox.Perform( msg, SB_ENDSCROLL, 0 );
  end
  else if MainPageControl.ActivePage = tsStyledPanels then
  begin
    PanelLeftScrollBox.Perform( msg, SB_ENDSCROLL, 0 );
    PanelRightScrollBox.Perform( msg, SB_ENDSCROLL, 0 );
  end;
end;

procedure TfmStyledButtonVCLStyles.SplitButtonsCheckBoxClick(Sender: TObject);
begin
  CreateAllButtons;
end;

procedure TfmStyledButtonVCLStyles.BuildStyleList;
var
  I: integer;
begin
  FStyleNames := TStringList.Create;
  for I := 0 to High(TStyleManager.StyleNames) do
    FStyleNames.Add(TStyleManager.StyleNames[i]);
  FStyleNames.Sorted := True;
end;

procedure TfmStyledButtonVCLStyles.ButtonClick(Sender: TObject);
begin
  if Sender is TButton then
    StyledShowMessage(TButton(Sender).Caption)
  else if Sender is TStyledButton then
    StyledShowMessage(TStyledButton(Sender).Caption);
end;

procedure TfmStyledButtonVCLStyles.ClearPanels(AScrollBox: TScrollBox);
begin
  //Clear previous Panels
  AScrollBox.Visible := False;
  try
    while AScrollBox.ControlCount > 0 do
      AScrollBox.Controls[AScrollBox.ControlCount-1].Free;
  finally
    AScrollBox.Visible := True;
  end;
end;

procedure TfmStyledButtonVCLStyles.CreateAllPanels(const AForce: Boolean = True);
var
  LPanelsInColumn: Integer;
  LContainerWidth, LWidth, LHeight: Integer;
begin
  if AForce then
    FPanelsInColumn := 0;
  GetPanelSize(LWidth, LHeight);
  LContainerWidth := PanelLeftPanel.Width - Round(VERT_SCROLL_WIDTH * GetScaleFactor);
  LPanelsInColumn := Trunc(LContainerWidth / GetScaleFactor) div (LWidth+PANEL_MARGIN);
  if LPanelsInColumn <> FPanelsInColumn then
  begin
    try
      CreatePanels(True, LPanelsInColumn);
      CreatePanels(False, LPanelsInColumn);
    finally
      FPanelsInColumn := LPanelsInColumn;
    end;
  end;
end;

procedure TfmStyledButtonVCLStyles.CreatePanels(const AVcl: Boolean;
  const APanelsInColumn: Integer);
var
  I, LRow: integer;
  LStyleName: string;
  LColumn: Integer;
  LWidth, LHeight: Integer;

  procedure CreateVCLPanel(AColumn, ATop: Integer;
    AStyleName: string);
  var
    LPanel: TPanel;
  begin
    LPanel := TPanel.Create(Self);
    LPanel.SetBounds((AColumn * LWidth) + (PANEL_MARGIN*AColumn),
      ATop, LWidth, LHeight);
    LPanel.ParentBackground := False; //Required to apply VCL style colors
    {$IFDEF D10_4+}
    LPanel.StyleName := AStyleName;
    {$ENDIF}
    if PanelShowCaptionCheckBox.Checked then
      LPanel.Caption := AStyleName
    else
      LPanel.Caption := '';
    LPanel.ShowCaption := PanelShowCaptionCheckBox.Checked;
    LPanel.Hint := AStyleName;
    LPanel.Parent := PanelLeftScrollBox;
    LPanel.PopupMenu := Self.PopupMenu;
    LPanel.OnClick := PanelClick;
  end;

  procedure CreateStyledPanel(AColumn, ATop: Integer;
    AStyleName: string);
  var
    LPanel: TStyledPanel;
    LAsVcl: Boolean;
  begin
    LAsVcl := PanelRenderRadioGroup.ItemIndex = RENDER_SAME_AS_VCL;
    LPanel := TStyledPanel.CreateStyled(Self,
      DEFAULT_CLASSIC_FAMILY,
      AStyleName,
      DEFAULT_APPEARANCE);

    LPanel.SetBounds((AColumn * LWidth) + (PANEL_MARGIN*AColumn),
      ATop, LWidth, LHeight);
    LPanel.ParentBackground := False; //Required to apply VCL style colors

    //Set AsVCLComponent based on render option
    if LAsVcl then
      LPanel.AsVCLComponent := True;

    case PanelRenderRadioGroup.ItemIndex of
      RENDER_ROUNDED: LPanel.StyleDrawType := btRounded; //All panels Rounded
      RENDER_ROUNDRECT: LPanel.StyleDrawType := btRoundRect; //All panels RoundRect
      RENDER_RECTANGLE: LPanel.StyleDrawType := btRect; //All panels Rect
    end;

    if PanelShowCaptionCheckBox.Checked then
      LPanel.Caption := AStyleName
    else
      LPanel.Caption := '';
    LPanel.ShowCaption := PanelShowCaptionCheckBox.Checked;

    if PanelOutlineCheckBox.Checked then
      LPanel.StyleAppearance := 'Outline'
    else
      LPanel.StyleAppearance := 'Normal';

    LPanel.Hint := AStyleName;
    LPanel.Parent := PanelRightScrollBox;
    LPanel.PopupMenu := Self.PopupMenu;
    LPanel.OnClick := PanelClick;
  end;

begin
  GetPanelSize(LWidth, LHeight);
  Screen.Cursor := crHourGlass;
  try
    //Clear previous Panels
    if AVcl then
      ClearPanels(PanelLeftScrollBox)
    else
      ClearPanels(PanelRightScrollBox);
    //Create VCL Panels or Styled Panels
    for I := 0 to FStyleNames.Count -1 do
    begin
      LStyleName := FStyleNames.Strings[I];
      LColumn := (I mod APanelsInColumn);
      LRow := (I div APanelsInColumn);
      if AVcl then
        CreateVCLPanel(LColumn, LRow*(LHeight+PANEL_MARGIN), LStyleName)
      else
        CreateStyledPanel(LColumn, LRow*(LHeight+PANEL_MARGIN), LStyleName)
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmStyledButtonVCLStyles.GetPanelSize(out AWidth, AHeight: Integer);
begin
  AWidth := PANEL_WIDTH;
  AHeight := PANEL_HEIGHT;
end;

procedure TfmStyledButtonVCLStyles.PanelClick(Sender: TObject);
begin
  if Sender is TPanel then
    StyledShowMessage(TPanel(Sender).Hint)
  else if Sender is TStyledPanel then
    StyledShowMessage(TStyledPanel(Sender).Hint);
end;

end.
