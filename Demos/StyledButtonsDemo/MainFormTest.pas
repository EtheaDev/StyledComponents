{******************************************************************************}
{                                                                              }
{       StyledComponents Library                                               }
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
unit MainFormTest;

interface

{$I ..\..\..\Source\StyledComponents.inc}

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
  System.ImageList,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  Vcl.BaseImageCollection,
  Vcl.StyledButton,
  Vcl.ButtonStylesAttributes,
  System.Actions,
  Vcl.ActnList,
  Vcl.StyledButtonEditorUnit,
  Vcl.ImageCollection,
  Vcl.Menus,
  Vcl.ComCtrls,
  DResources;

const
  BUTTON_HEIGHT = 28;
  BUTTON_WIDTH = 140;
  BUTTON_MARGIN = 4;
  BUTTON_COL_COUNT = 24;

  RENDER_SAME_AS_VCL = 0;
  RENDER_ROUNDED = 1;
  RENDER_RECTANGLE = 2;
  RENDER_FAB = 3;

type
  TTestMainForm = class(TForm)
    ActionList: TActionList;
    TestAction: TAction;
    PopupMenu: TPopupMenu;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Exit1: TMenuItem;
    TopPanel: TPanel;
    StyleLabel: TLabel;
    cbChangeStyle: TComboBox;
    ShowEditButton: TStyledButton;
    AboutButton: TButton;
    LeftPanel: TPanel;
    LeftScrollBox: TScrollBox;
    RightPanel: TPanel;
    RightScrollBox: TScrollBox;
    Panel1: TPanel;
    TopRightPanel: TPanel;
    RenderRadioGroup: TRadioGroup;
    VirtualImageList32: TVirtualImageList;
    SplitButtonsCheckBox: TCheckBox;
    EnabledCheckBox: TCheckBox;
    procedure TestActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbChangeStyleSelect(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure RenderRadioGroupClick(Sender: TObject);
    procedure PopUpMenuClick(Sender: TObject);
    procedure SplitButtonsCheckBoxClick(Sender: TObject);
    procedure AboutButtonClick(Sender: TObject);
    procedure EnabledCheckBoxClick(Sender: TObject);
  private
    FStyleNames: TStringList;
    FSplitButtons: Boolean;
    procedure GetButtonSize(out AWidth, AHeight: Integer);
    procedure BuildStyleList;
    procedure CreateVCLButtons;
    procedure CreateStyledButtons;
    procedure ClearButtons(AScrollBox: TScrollBox);
    procedure ButtonClick(Sender: TObject);
  protected
  end;

var
  TestMainForm: TTestMainForm;

implementation

{$R *.dfm}

uses
  System.TypInfo
  , Vcl.StandardButtonStyles
  , Vcl.Themes
  , WinApi.ShellAPI
  , FAboutForm
  ;

{ TMainForm }

procedure TTestMainForm.cbChangeStyleSelect(Sender: TObject);
begin
  TStyleManager.TrySetStyle(cbChangeStyle.Text);
end;

procedure TTestMainForm.ClearButtons(AScrollBox: TScrollBox);
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

procedure TTestMainForm.CreateStyledButtons;
var
  I, X: integer;
  LStyleName: string;
  LColumn: Integer;
  LWidth, LHeight: Integer;

  procedure CreateStyledButton(AColumn, ATop: Integer;
    AStyleName: string);
  begin
    With TStyledButton.CreateStyled(Self,
      DEFAULT_CLASSIC_FAMILY,
      AStyleName,
      DEFAULT_APPEARANCE) do
    begin
      Enabled := EnabledCheckBox.Checked;
      if RenderRadioGroup.ItemIndex <> RENDER_FAB then
      begin
        case RenderRadioGroup.ItemIndex of
          RENDER_ROUNDED: StyleDrawType := btRounded; //All buttons Rounded
          RENDER_RECTANGLE: StyleDrawType := btRect; //All buttons Rect
        end;
        Caption := AStyleName;
        if FSplitButtons then
        begin
          Style := bsSplitButton;
          DropDownMenu := Self.PopupMenu;
        end;
      end
      else
      begin
        //Render FAB button
        StyleDrawType := btEllipse;
        Images := VirtualImageList32;
        ImageAlignment := iaCenter;
        ImageIndex := I mod VirtualImageList32.Count;
      end;
      Hint := AStyleName;
      SetBounds((AColumn * LWidth) + (BUTTON_MARGIN*AColumn),
        ATop, LWidth, LHeight);
      Parent := RightScrollBox;
      PopupMenu := Self.PopupMenu;
      OnClick := ButtonClick;
    end;
  end;

begin
  GetButtonSize(LWidth, LHeight);
  Screen.Cursor := crHourGlass;
  try
    //Clear previous Buttons
    ClearButtons(RightScrollBox);
    //Create Styled Buttons
    X := 0;
    LColumn := 0;
    for I := 0 to FStyleNames.Count -1 do
    begin
      LStyleName := FStyleNames.Strings[I];
      if (I div BUTTON_COL_COUNT) <> LColumn then
      begin
        LColumn := (I div BUTTON_COL_COUNT);
        X := 0;
      end;
      CreateStyledButton(LColumn, X*(LHeight+BUTTON_MARGIN), LStyleName);
      Inc(X);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TTestMainForm.CreateVCLButtons;
var
  I, X: integer;
  LStyleName: string;
  LColumn: Integer;
  LWidth, LHeight: Integer;

  procedure CreateVCLButton(AColumn, ATop: Integer;
    AStyleName: string);
  begin
    With TButton.Create(Self) do
    begin
      Enabled := EnabledCheckBox.Checked;
      SetBounds((AColumn * LWidth) + (BUTTON_MARGIN*AColumn),
        ATop, LWidth, LHeight);
      StyleName := AStyleName;
      Caption := AStyleName;
      Hint := AStyleName;
      Parent := LeftScrollBox;
      PopupMenu := Self.PopupMenu;
      OnClick := ButtonClick;
      if FSplitButtons then
      begin
        Style := TButtonStyle.bsSplitButton;
        DropDownMenu := Self.PopupMenu;
      end;
    end;
  end;

begin
  GetButtonSize(LWidth, LHeight);
  Screen.Cursor := crHourGlass;
  try
    //Clear previous Buttons
    ClearButtons(LeftScrollBox);
    //Create VCL Buttons
    X := 0;
    LColumn := 0;
    for I := 0 to FStyleNames.Count -1 do
    begin
      LStyleName := FStyleNames.Strings[I];
      if (I div BUTTON_COL_COUNT) <> LColumn then
      begin
        LColumn := (I div BUTTON_COL_COUNT);
        X := 0;
      end;
      CreateVCLButton(LColumn, X*(LHeight+BUTTON_MARGIN), LStyleName);
      Inc(X);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TTestMainForm.EnabledCheckBoxClick(Sender: TObject);
begin
  CreateVCLButtons;
  CreateStyledButtons;
end;

procedure TTestMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Caption := Application.Title + ' - ' + Caption;
  BuildStyleList;
  CreateVCLButtons;
  CreateStyledButtons;
end;

procedure TTestMainForm.FormDestroy(Sender: TObject);
begin
  FStyleNames.Free;
end;

procedure TTestMainForm.FormResize(Sender: TObject);
begin
  LeftPanel.Width := ClientWidth div 2;
end;

procedure TTestMainForm.GetButtonSize(out AWidth, AHeight: Integer);
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
end;

procedure TTestMainForm.PopUpMenuClick(Sender: TObject);
begin
  ShowMessage((Sender as TMenuItem).Caption);
end;

procedure TTestMainForm.RenderRadioGroupClick(Sender: TObject);
begin
  CreateVCLButtons;
  CreateStyledButtons;
end;

procedure TTestMainForm.ScrollBoxMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
Var
  msg: Cardinal;
  code: Cardinal;
  i, n: Integer;
begin
  Handled := true;
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

procedure TTestMainForm.SplitButtonsCheckBoxClick(Sender: TObject);
begin
  FSplitButtons := SplitButtonsCheckBox.Checked;
  CreateVCLButtons;
  CreateStyledButtons;
end;

procedure TTestMainForm.TestActionExecute(Sender: TObject);
begin
  EditStyledButton(ShowEditButton);
end;

procedure TTestMainForm.AboutButtonClick(Sender: TObject);
begin
  ShowAboutForm;
end;

procedure TTestMainForm.BuildStyleList;
var
  I, SelectedIndex: integer;
  LStyleName, LActiveStyleName: string;
begin
  FStyleNames := TStringList.Create;
  for I := 0 to High(TStyleManager.StyleNames) do
    FStyleNames.Add(TStyleManager.StyleNames[i]);
  FStyleNames.Sorted := True;
  SelectedIndex := -1;
  cbChangeStyle.Items.Clear;
  LActiveStyleName := TStyleManager.ActiveStyle.Name;
  for i := 0 to FStyleNames.Count -1 do
  begin
    LStyleName := FStyleNames.Strings[I];
    cbChangeStyle.Items.Add(LStyleName);
    if SameText(LStyleName, LActiveStyleName)  then
      SelectedIndex := i;
  end;
  cbChangeStyle.ItemIndex := SelectedIndex;
end;

procedure TTestMainForm.ButtonClick(Sender: TObject);
begin
  if Sender is TButton then
    ShowMessage(TButton(Sender).Caption)
  else if Sender is TStyledButton then
    ShowMessage(TStyledButton(Sender).Caption);
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
end.
