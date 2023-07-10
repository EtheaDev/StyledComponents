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
unit MainFormOld;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ButtonStylesAttributes,
  Vcl.StandardButtonStyles, Vcl.AngularButtonStyles, Vcl.BootstrapButtonStyles, Vcl.ColorButtonStyles,
  Vcl.StyledButton, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ToolWin, Vcl.ActnMan,
  Vcl.ActnCtrls, Vcl.ImgList, Vcl.StdCtrls, Vcl.StyledToolbar;

const
  //Params to check
  SHOW_CAPTIONS = False;
  BUTTON_WIDTH = 60;
  BUTTON_HEIGHT = 60;
  TOOLBAR_AUTOSIZE = True;

type
  TfmMain = class(TForm)
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    StyledToolbar1: TStyledToolbar;
    StyledToolButton1: TStyledToolButton;
    StyledToolButton2: TStyledToolButton;
    StyledToolButton3: TStyledToolButton;
    SSep1: TStyledToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    StyledToolButton5: TStyledToolButton;
    BottomPanel: TPanel;
    ImageList32: TImageList;
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
    procedure FormCreate(Sender: TObject);
    procedure ToolBarClick(Sender: TObject);
    procedure ToolButtonclick(Sender: TObject);
    procedure CreateButtonClick(Sender: TObject);
    procedure UpdateToolbars(Sender: TObject);
  private
    FToolBar: TToolBar;
    FStyledToolBar: TStyledToolBar;
    procedure CreateStyledToolBar;
    procedure CreateToolbars;
    procedure CreateToolBar;
    function AddStyledButtonToToolbar(var bar: TStyledToolBar;
      const Caption: string; const Style: TStyledToolButtonStyle;
      ImageIndex: Integer = 0;
      const AFamily: TStyledButtonFamily = '';
      const AClass: TStyledButtonClass = '';
      const AAppearance: TStyledButtonAppearance = ''): TStyledToolButton;
    function AddButtonToToolbar(var bar: TToolBar; caption: string;
      Style: TToolButtonStyle; ImageIndex: Integer): TToolButton;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

function TfmMain.AddStyledButtonToToolbar(var bar: TStyledToolBar;
  const Caption: string;
  const Style: TStyledToolButtonStyle;
  ImageIndex: Integer = 0;
  const AFamily: TStyledButtonFamily = '';
  const AClass: TStyledButtonClass = '';
  const AAppearance: TStyledButtonAppearance = ''): TStyledToolButton;
begin
  if bar.NewButton(Result, Style) then
  begin
    Result.Name := Caption;
    Result.Caption := caption;
    Result.Hint := Format('Hint of %s button.',[caption]);
    Result.ImageIndex := ImageIndex;
    Result.SetButtonStyle(AFamily, AClass, AAppearance);
    Result.OnClick := ToolButtonclick;
  end;
end;

function TfmMain.AddButtonToToolbar(var bar: TToolBar; caption: string;
  Style: TToolButtonStyle; ImageIndex: Integer): TToolButton;
var
  lastbtnidx: integer;
begin
  Result := TToolButton.Create(Self);
  Try
    Result.Style := Style;
    Result.Caption := caption;
    Result.Hint := Format('Hint of %s button.',[caption]);
    Result.ImageIndex := ImageIndex;
    lastbtnidx := bar.ButtonCount - 1;
    if lastbtnidx > -1 then
      Result.Left := bar.Buttons[lastbtnidx].Left + 1
    else
      Result.Left := 0;
    Result.Parent := bar;
    Result.OnClick := ToolButtonclick;
    if Style in [tbsSeparator, tbsDivider] then
      Result.Width := 10;
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
  FStyledToolBar.AutoWrap := True;
  FStyledToolBar.Images := ImageList32;
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
  FToolBar.Images := ImageList32;
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
  AddButtonToToolbar(FToolBar, 'Caption3', tbsButton, 10);
  AddButtonToToolbar(FToolBar, '', tbsSeparator, -1);
  AddButtonToToolbar(FToolBar, 'Caption4', tbsButton, 15);

  //Create Styled Toolbar
  CreateStyledToolBar;
  //Add StyledButtons, Separators and Dividers with Styles
  AddStyledButtonToToolbar(FStyledToolBar, 'Caption1', tbsStyledButton, 0,
    BOOTSTRAP_FAMILY, btn_primary, BOOTSTRAP_OUTLINE);

  AddStyledButtonToToolbar(FStyledToolBar, 'Caption2', tbsStyledButton, 5,
    BOOTSTRAP_FAMILY, btn_secondary, BOOTSTRAP_OUTLINE);

  AddStyledButtonToToolbar(FStyledToolBar, '', tbsStyledDivider);

  AddStyledButtonToToolbar(FStyledToolBar, 'Caption3', tbsStyledButton, 10,
    BOOTSTRAP_FAMILY, btn_success, BOOTSTRAP_OUTLINE);

  AddStyledButtonToToolbar(FStyledToolBar, '', tbsStyledSeparator);

  LButton := AddStyledButtonToToolbar(FStyledToolBar, 'Caption4', tbsStyledButton, 15,
    BOOTSTRAP_FAMILY, btn_danger, BOOTSTRAP_OUTLINE);

  tbWidth.Position := BUTTON_WIDTH;
  tbHeight.Position := BUTTON_HEIGHT;

  //StyledToolbar1.NewButton(LButton);
  //LButton.Caption := 'Button';
  //StyledToolbar1.SetToolbarStyle(BOOTSTRAP_FAMILY, btn_warning, BOOTSTRAP_OUTLINE);
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  ShowCaptionCheckBox.Checked := SHOW_CAPTIONS;
end;

procedure TfmMain.ToolBarClick(Sender: TObject);
begin
  ;
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
    FStyledToolBar.ShowCaptions := ShowCaptionCheckBox.Checked;
    FStyledToolBar.ButtonWidth := tbWidth.Position;
    FStyledToolBar.ButtonHeight := tbHeight.Position;
    FStyledToolBar.List := ListCheckBox.Checked;
  end;
  if Assigned(FToolBar) then
  begin
    FToolBar.ShowCaptions := ShowCaptionCheckBox.Checked;
    FToolBar.ButtonWidth := tbWidth.Position;
    FToolBar.ButtonHeight := tbHeight.Position;
    FToolBar.List := ListCheckBox.Checked;
  end;
end;

initialization
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

end.
