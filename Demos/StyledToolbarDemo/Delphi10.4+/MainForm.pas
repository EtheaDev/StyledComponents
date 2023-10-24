{******************************************************************************}
{                                                                              }
{       TStyledToolbar Test                                                    }
{       Comparison TStyledToolbar with TToolBar                                }
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
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ButtonStylesAttributes,
  Vcl.StandardButtonStyles, Vcl.AngularButtonStyles, Vcl.BootstrapButtonStyles, Vcl.ColorButtonStyles,
  Vcl.StyledButton, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ToolWin, Vcl.ActnMan,
  Vcl.ActnCtrls, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList,
  DResources, Vcl.StdCtrls, Vcl.StyledToolbar, Vcl.Menus;

const
  //Params to check
  SHOW_CAPTIONS = True;
  BUTTON_WIDTH = 60;
  BUTTON_HEIGHT = 60;
  TOOLBAR_AUTOSIZE = True;

type
  TfmMain = class(TForm)
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    VirtualImageList: TVirtualImageList;
    ToolButton4: TToolButton;
    StyledToolbar: TStyledToolbar;
    StyledToolButton1: TStyledToolButton;
    StyledToolButton2: TStyledToolButton;
    StyledToolButton3: TStyledToolButton;
    SSep1: TStyledToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    StyledToolButton5: TStyledToolButton;
    BottomPanel: TPanel;
    StyledToolButton6: TStyledToolButton;
    ClientPanel: TPanel;
    LeftPanel: TPanel;
    CreateButton: TStyledButton;
    BottomClientPanel: TPanel;
    WidthLabel: TLabel;
    HeightLabel: TLabel;
    tbWidth: TTrackBar;
    tbHeight: TTrackBar;
    ShowCaptionCheckBox: TCheckBox;
    ListCheckBox: TCheckBox;
    PopupMenu: TPopupMenu;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Exit1: TMenuItem;
    StyledToolButton4: TStyledToolButton;
    ToolButton7: TToolButton;
    Panel1: TPanel;
    cbChangeStyle: TComboBox;
    StyleLabel: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    StyledToolButton7: TStyledToolButton;
    ToolButton8: TToolButton;
    FlatCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ToolBarClick(Sender: TObject);
    procedure ToolButtonclick(Sender: TObject);
    procedure CreateButtonClick(Sender: TObject);
    procedure UpdateToolbars(Sender: TObject);
    procedure PopUpMenuClick(Sender: TObject);
    procedure cbChangeStyleSelect(Sender: TObject);
    procedure ToolBarMouseEnter(Sender: TObject);
    procedure StyledToolbarMouseEnter(Sender: TObject);
  private
    FToolBar: TToolBar;
    FStyledToolBar: TStyledToolBar;
    procedure CreateStyledToolBar;
    procedure CreateToolbars;
    procedure CreateToolBar;
    function AddStyledButtonToToolbar(var AToolbar: TStyledToolBar;
      const ACaption: string; const AStyle: TToolButtonStyle;
      AImageIndex: Integer = 0;
      const AFamily: TStyledButtonFamily = '';
      const AClass: TStyledButtonClass = '';
      const AAppearance: TStyledButtonAppearance = ''): TStyledToolButton;
    function AddButtonToToolbar(var AToolbar: TToolBar; ACaption: string;
      AStyle: TToolButtonStyle; AImageIndex: Integer): TToolButton;
    procedure BuildStyleList;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

uses
  Themes,
  Vcl.StyledButtonEditorUnit;

function TfmMain.AddStyledButtonToToolbar(var AToolbar: TStyledToolBar;
  const ACaption: string;
  const AStyle: TToolButtonStyle;
  AImageIndex: Integer = 0;
  const AFamily: TStyledButtonFamily = '';
  const AClass: TStyledButtonClass = '';
  const AAppearance: TStyledButtonAppearance = ''): TStyledToolButton;
begin
  if AToolbar.NewButton(Result, AStyle) then
  begin
    Result.Name := ACaption;
    Result.Caption := ACaption;
    Result.Hint := Format('Hint of %s button.',[ACaption]);
    Result.ImageIndex := AImageIndex;
    Result.SetButtonStyle(AFamily, AClass, AAppearance);
    Result.OnClick := ToolButtonclick;
  end;
end;

function TfmMain.AddButtonToToolbar(var AToolbar: TToolBar; ACaption: string;
  AStyle: TToolButtonStyle; AImageIndex: Integer): TToolButton;
var
  lastbtnidx: integer;
begin
  Result := TToolButton.Create(Self);
  Try
    Result.Style := AStyle;
    Result.Caption := ACaption;
    Result.Hint := Format('Hint of %s button.',[ACaption]);
    Result.ImageIndex := AImageIndex;
    lastbtnidx := AToolbar.ButtonCount - 1;
    if lastbtnidx > -1 then
      Result.Left := AToolbar.Buttons[lastbtnidx].Left + 1
    else
      Result.Left := 0;
    Result.Parent := AToolbar;
    Result.OnClick := ToolButtonclick;
    if AStyle in [tbsSeparator, tbsDivider] then
      Result.Width := 8;
  Except
    Result.Free;
    raise;
  End;
end;

procedure TfmMain.CreateButtonClick(Sender: TObject);
begin
  CreateToolbars;
  LeftPanel.Visible := False;
  BottomClientPanel.Visible := True;
end;

procedure TfmMain.CreateStyledToolBar;
begin
  FStyledToolBar := TStyledToolBar.Create(Self);
  FStyledToolBar.ShowCaptions := SHOW_CAPTIONS;
  FStyledToolBar.Caption := 'StyledToolBar';
  FStyledToolBar.Parent := Self;
  FStyledToolBar.AutoSize := TOOLBAR_AUTOSIZE;
  FStyledToolBar.Wrapable := not TOOLBAR_AUTOSIZE;
  FStyledToolBar.Images := VirtualImageList;
  FStyledToolBar.ButtonWidth := BUTTON_WIDTH;
  FStyledToolBar.ButtonHeight := BUTTON_HEIGHT;
  if not FStyledToolBar.AutoSize then
    FStyledToolBar.Height := BUTTON_HEIGHT;
  FStyledToolBar.OnClick := ToolBarClick;
end;

procedure TfmMain.CreateToolBar;
begin
  FToolBar := TToolBar.Create(Self);
  FToolBar.Flat := True;
  FToolBar.ShowCaptions := SHOW_CAPTIONS;
  FToolBar.Caption := 'ToolBar';
  FToolBar.Parent := Self;
  FToolBar.AutoSize := TOOLBAR_AUTOSIZE;
  FToolBar.Wrapable := True;
  FToolBar.Images := VirtualImageList;
  FToolBar.StyleElements := [];
  FToolBar.ButtonWidth := BUTTON_WIDTH;
  FToolBar.ButtonHeight := BUTTON_HEIGHT;
  if not FToolBar.AutoSize then
    FToolBar.Height := BUTTON_HEIGHT;
  FToolBar.OnClick := ToolBarClick;
end;

procedure TfmMain.CreateToolbars;
var
  LButton: TStyledToolButton;
begin
  //Create Standard Toolbar
  CreateToolBar;
  //Add Buttons, Separators and Dividers
  AddButtonToToolbar(FToolBar, 'Caption1', tbsButton, 0);
  AddButtonToToolbar(FToolBar, 'Caption2', tbsButton, 5);
  AddButtonToToolbar(FToolBar, '', tbsSeparator, -1);
  AddButtonToToolbar(FToolBar, 'Caption3', tbsButton, 8);
  AddButtonToToolbar(FToolBar, '', tbsSeparator, -1);
  AddButtonToToolbar(FToolBar, 'Caption4', tbsButton, 10);

  //Create Styled Toolbar
  CreateStyledToolBar;
  //Add StyledButtons, Separators and Dividers with Styles
  AddStyledButtonToToolbar(FStyledToolBar, 'Caption1', tbsButton, 0,
    BOOTSTRAP_FAMILY, btn_primary, BOOTSTRAP_OUTLINE);

  AddStyledButtonToToolbar(FStyledToolBar, 'Caption2', tbsButton, 5,
    BOOTSTRAP_FAMILY, btn_secondary, BOOTSTRAP_OUTLINE);

  AddStyledButtonToToolbar(FStyledToolBar, '', tbsDivider);

  AddStyledButtonToToolbar(FStyledToolBar, 'Caption3', tbsButton, 8,
    BOOTSTRAP_FAMILY, btn_success, BOOTSTRAP_OUTLINE);

  AddStyledButtonToToolbar(FStyledToolBar, '', tbsSeparator);

  LButton := AddStyledButtonToToolbar(FStyledToolBar, 'Caption4', tbsButton, 10,
    BOOTSTRAP_FAMILY, btn_danger, BOOTSTRAP_OUTLINE);

  tbWidth.Position := BUTTON_WIDTH;
  tbHeight.Position := BUTTON_HEIGHT;

  //StyledToolbar1.NewButton(LButton);
  //LButton.Caption := 'Button';
  //StyledToolbar1.SetToolbarStyle(BOOTSTRAP_FAMILY, btn_warning, BOOTSTRAP_OUTLINE);
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  BuildStyleList;
  Caption := Application.Title;
  ShowCaptionCheckBox.Checked := SHOW_CAPTIONS;
end;

procedure TfmMain.PopUpMenuClick(Sender: TObject);
begin
  ShowMessage((Sender as TMenuItem).Caption);
end;

procedure TfmMain.StyledToolbarMouseEnter(Sender: TObject);
begin
  Caption := Format('StyledToolbar - ButtonWidth: %d - ButtonHeight: %d',
    [StyledToolBar.ButtonWidth, StyledToolBar.ButtonHeight]);
end;

procedure TfmMain.ToolBarMouseEnter(Sender: TObject);
begin
  Caption := Format('Toolbar - ButtonWidth: %d - ButtonHeight: %d',
    [ToolBar.ButtonWidth, ToolBar.ButtonHeight]);
end;

procedure TfmMain.ToolBarClick(Sender: TObject);
begin
  if Sender is TToolbar then
    ShowMessage(TToolbar(Sender).Caption)
  else if Sender is TStyledToolbar then
    ShowMessage(TStyledToolbar(Sender).Caption);
end;

procedure TfmMain.ToolButtonclick(Sender: TObject);
begin
  if Sender is TToolButton then
    ShowMessage(TToolButton(Sender).Caption)
  else if Sender is TStyledToolButton then
    ShowMessage(TStyledToolButton(Sender).Caption);
end;

procedure TfmMain.UpdateToolbars(Sender: TObject);
begin
  if Assigned(FStyledToolBar) then
  begin
    FStyledToolBar.Flat := FlatCheckBox.Checked;
    FStyledToolBar.ShowCaptions := ShowCaptionCheckBox.Checked;
    FStyledToolBar.ButtonWidth := tbWidth.Position;
    FStyledToolBar.ButtonHeight := tbHeight.Position;
    FStyledToolBar.List := ListCheckBox.Checked;
  end;
  if Assigned(FToolBar) then
  begin
    FToolBar.Flat := FlatCheckBox.Checked;
    FToolBar.ShowCaptions := ShowCaptionCheckBox.Checked;
    FToolBar.ButtonWidth := tbWidth.Position;
    FToolBar.ButtonHeight := tbHeight.Position;
    FToolBar.List := ListCheckBox.Checked;
  end;
  ToolBar.List := ListCheckBox.Checked;
  ToolBar.ShowCaptions := ShowCaptionCheckBox.Checked;
  ToolBar.Flat := FlatCheckBox.Checked;

  StyledToolBar.List := ListCheckBox.Checked;
  StyledToolBar.ShowCaptions := ShowCaptionCheckBox.Checked;
  StyledToolBar.Flat := FlatCheckBox.Checked;

end;

procedure TfmMain.BuildStyleList;
var
  i, SelectedIndex: integer;
  LStyleName, LActiveStyleName: string;
begin
  SelectedIndex := -1;
  cbChangeStyle.Items.Clear;
  LActiveStyleName := TStyleManager.ActiveStyle.Name;
  for i := 0 to High(TStyleManager.StyleNames) do
  begin
    LStyleName := TStyleManager.StyleNames[i];
    cbChangeStyle.Items.Add(LStyleName);
    if SameText(LStyleName, LActiveStyleName)  then
      SelectedIndex := i;
  end;
  cbChangeStyle.ItemIndex := SelectedIndex;
end;

procedure TfmMain.cbChangeStyleSelect(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    TStyleManager.TrySetStyle(cbChangeStyle.Text);
  finally
    Screen.Cursor := crDefault;
  end;
end;

initialization
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

end.
