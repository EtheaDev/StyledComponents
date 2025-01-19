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
unit StyledButtonVCLStylesForm;

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
  Vcl.ButtonStylesAttributes,
  Vcl.ActnList,
  Vcl.StyledButtonEditorUnit,
  Vcl.Menus,
  Vcl.ComCtrls;

const
  BUTTON_HEIGHT = 28;
  BUTTON_WIDTH = 140;
  BUTTON_MARGIN = 4;
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
    TopPanel: TPanel;
    LeftPanel: TPanel;
    LeftScrollBox: TScrollBox;
    RightPanel: TPanel;
    RightScrollBox: TScrollBox;
    Panel1: TPanel;
    TopRightPanel: TPanel;
    RenderRadioGroup: TRadioGroup;
    StyleRadioGroup: TRadioGroup;
    GroupBox1: TGroupBox;
    EnabledCheckBox: TCheckBox;
    OutlineCheckBox: TCheckBox;
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
    procedure CreateAllButtons(const AForce: Boolean = True);
    function GetScaleFactor: Single;
    procedure GetButtonSize(out AWidth, AHeight: Integer);
    procedure BuildStyleList;
    procedure CreateButtons(const AVcl: Boolean; const AButtonsInColumn: Integer);
    procedure ClearButtons(AScrollBox: TScrollBox);
    procedure ButtonClick(Sender: TObject);
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
  CreateAllButtons;
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
  if LeftPanel.Width <> ClientWidth div 2 then
  begin
    LeftPanel.Width := ClientWidth div 2;
    CreateAllButtons(False);
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
  CreateAllButtons;
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
    LeftScrollBox.Perform( msg, code, 0 );
    RightScrollBox.Perform( msg, code, 0 );
  end;
  LeftScrollBox.Perform( msg, SB_ENDSCROLL, 0 );
  RightScrollBox.Perform( msg, SB_ENDSCROLL, 0 );
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

end.
