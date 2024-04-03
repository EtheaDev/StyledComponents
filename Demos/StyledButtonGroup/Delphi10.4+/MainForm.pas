{******************************************************************************}
{                                                                              }
{       TStyledButtonGroup Demo                                                }
{       Comparison TStyledButtonGroup with TButtonGroup                        }
{                                                                              }
{       Copyright (c) 2022-2024 (Ethea S.r.l.)                                 }
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
  DResources, Vcl.StdCtrls, Vcl.Menus, Vcl.StyledButtonGroup,
  Vcl.ButtonGroup, System.Actions, Vcl.ActnList, Vcl.StdActns;

const
  //Params to check
  CAPTION_STR = 'Caption';
  BUTTON_WIDTH = 120;
  BUTTON_HEIGHT = 40;
  BUTTONGROUP_FULLSIZE = True;
  BUTTONGROUP_SHOW_CAPTIONS = True;
  BUTTON_GROUP_GROUP_STYLE = False;

type
  TfmMain = class(TForm)
    VirtualImageList: TVirtualImageList;
    BottomPanel: TPanel;
    ClientPanel: TPanel;
    LeftPanel: TPanel;
    CreateButton: TStyledButton;
    BottomClientPanel: TPanel;
    WidthLabel: TLabel;
    HeightLabel: TLabel;
    tbWidth: TTrackBar;
    tbHeight: TTrackBar;
    ShowCaptionCheckBox: TCheckBox;
    FullSizeCheckBox: TCheckBox;
    PopupMenu: TPopupMenu;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Exit1: TMenuItem;
    Panel1: TPanel;
    cbChangeStyle: TComboBox;
    StyleLabel: TLabel;
    ShowIconCheckBox: TCheckBox;
    ButtonGroup: TButtonGroup;
    ActionList: TActionList;
    acFileOpen: TFileOpen;
    StyledButtonGroup: TStyledButtonGroup;
    StyledButtonGroupBootstrap: TStyledButtonGroup;
    cbCaptionAlignment: TComboBox;
    cbImageAlignment: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure CreateButtonClick(Sender: TObject);
    procedure UpdateButtonGroups(Sender: TObject);
    procedure PopUpMenuClick(Sender: TObject);
    procedure cbChangeStyleSelect(Sender: TObject);
    procedure ButtonGroupButtonClicked(Sender: TObject; Index: Integer);
    procedure cbCaptionAlignmentSelect(Sender: TObject);
    procedure cbImageAlignmentSelect(Sender: TObject);
  private
    FButtonGroup: TButtonGroup;
    FStyledButtonGroup: TStyledButtonGroup;
    procedure CreateStyledButtonGroup;
    procedure CreateButtonGroups;
    procedure CreateButtonGroup;
    function AddStyledButtonToButtonGroup(
      var AButtonGroup: TStyledButtonGroup;
      const ACaption: string; const AImageIndex: Integer = 0;
      const AFamily: TStyledButtonFamily = '';
      const AClass: TStyledButtonClass = '';
      const AAppearance: TStyledButtonAppearance = ''): TStyledGrpButtonItem;
    function AddButtonToButtonGroup(var AButtonGroup: TButtonGroup;
      const ACaption: string; const AImageIndex: Integer): TGrpButtonItem;
    procedure BuildStyleList;
    procedure BuildImageAlignment;
    procedure BuildCaptionAligmentList;
    procedure UpdateShowCaptions(const AButtonGroup: TButtonGroup;
      const AShow: Boolean);
    procedure UpdateSetFullSize(const AButtonGroup: TButtonGroup;
      const AFullSize: Boolean);
    procedure UpdateSetGroupStyle(const AButtonGroup: TButtonGroup;
      const AGroupStyle: Boolean);
    procedure UpdateShowIcon(const AButtonGroup: TButtonGroup;
      const AShowIcon: Boolean);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

uses
  System.TypInfo,
  Vcl.Themes,
  Vcl.StyledButtonEditorUnit;

function TfmMain.AddStyledButtonToButtonGroup(
  var AButtonGroup: TStyledButtonGroup;
  const ACaption: string; const AImageIndex: Integer = 0;
  const AFamily: TStyledButtonFamily = '';
  const AClass: TStyledButtonClass = '';
  const AAppearance: TStyledButtonAppearance = ''): TStyledGrpButtonItem;
begin
  Result := AButtonGroup.Items.Add;
  Result.Caption := ACaption;
  Result.Hint := Format('Hint of %s button.',[ACaption]);
  Result.ImageIndex := AImageIndex;
  Result.SetButtonStyle(AFamily, AClass, AAppearance);
end;

function TfmMain.AddButtonToButtonGroup(var AButtonGroup: TButtonGroup;
  const ACaption: string; const AImageIndex: Integer): TGrpButtonItem;
begin
  Result := AButtonGroup.Items.Add;
  Result.Caption := ACaption;
  Result.Hint := Format('Hint of %s button.',[ACaption]);
  Result.ImageIndex := AImageIndex;
end;

procedure TfmMain.CreateButtonClick(Sender: TObject);
begin
  CreateButtonGroups;
  LeftPanel.Visible := False;
  BottomClientPanel.Visible := True;
end;

procedure TfmMain.UpdateShowCaptions(const AButtonGroup: TButtonGroup;
  const AShow: Boolean);
begin
  if AShow then
    AButtonGroup.ButtonOptions := AButtonGroup.ButtonOptions + [gboShowCaptions]
  else
    AButtonGroup.ButtonOptions := AButtonGroup.ButtonOptions - [gboShowCaptions];
end;

procedure TfmMain.UpdateShowIcon(const AButtonGroup: TButtonGroup;
  const AShowIcon: Boolean);
begin
  if AShowIcon then
    AButtonGroup.Images := VirtualImageList
  else
    AButtonGroup.Images := nil;
end;

procedure TfmMain.UpdateSetFullSize(const AButtonGroup: TButtonGroup;
  const AFullSize: Boolean);
begin
  if AFullSize then
    AButtonGroup.ButtonOptions := AButtonGroup.ButtonOptions + [gboFullSize]
  else
    AButtonGroup.ButtonOptions := AButtonGroup.ButtonOptions - [gboFullSize];
end;

procedure TfmMain.UpdateSetGroupStyle(const AButtonGroup: TButtonGroup;
  const AGroupStyle: Boolean);
begin
  if AGroupStyle then
    AButtonGroup.ButtonOptions := AButtonGroup.ButtonOptions + [gboGroupStyle]
  else
    AButtonGroup.ButtonOptions := AButtonGroup.ButtonOptions - [gboGroupStyle];
end;

procedure TfmMain.CreateStyledButtonGroup;
begin
  FStyledButtonGroup := TStyledButtonGroup.Create(Self);
  FStyledButtonGroup.Parent := Self;
  FStyledButtonGroup.Align := alLeft;
  FStyledButtonGroup.Width := Round(BUTTON_WIDTH * 1.5);
  UpdateShowCaptions(FStyledButtonGroup, BUTTONGROUP_SHOW_CAPTIONS);
  UpdateSetFullSize(FStyledButtonGroup, BUTTONGROUP_FULLSIZE);
  UpdateSetGroupStyle(FStyledButtonGroup, BUTTON_GROUP_GROUP_STYLE);
  FStyledButtonGroup.Images := VirtualImageList;
  FStyledButtonGroup.ButtonWidth := BUTTON_WIDTH;
  FStyledButtonGroup.ButtonHeight := BUTTON_HEIGHT;
end;

procedure TfmMain.CreateButtonGroup;
begin
  FButtonGroup := TButtonGroup.Create(Self);
  FButtonGroup.Parent := Self;
  FButtonGroup.Align := alLeft;
  FButtonGroup.Width := Round(BUTTON_WIDTH * 1.5);
  UpdateShowCaptions(FButtonGroup, BUTTONGROUP_SHOW_CAPTIONS);
  UpdateSetFullSize(FButtonGroup, BUTTONGROUP_FULLSIZE);
  UpdateSetGroupStyle(FButtonGroup, BUTTON_GROUP_GROUP_STYLE);
  FButtonGroup.Images := VirtualImageList;
  FButtonGroup.ButtonWidth := BUTTON_WIDTH;
  FButtonGroup.ButtonHeight := BUTTON_HEIGHT;
end;

procedure TfmMain.CreateButtonGroups;
var
  LButton: TGrpButtonItem;
begin
  //Create Standard ButtonGroup
  CreateButtonGroup;
  //Add Buttons, Separators and Dividers
  AddButtonToButtonGroup(FButtonGroup, CAPTION_STR+'1', 0);
  AddButtonToButtonGroup(FButtonGroup, CAPTION_STR+'2', 5);
  AddButtonToButtonGroup(FButtonGroup, CAPTION_STR+'3', 8);
  AddButtonToButtonGroup(FButtonGroup, CAPTION_STR+'4', 10);

  //Create Styled ButtonGroup
  CreateStyledButtonGroup;
  //Add StyledButtons, Separators and Dividers with Styles
  AddStyledButtonToButtonGroup(FStyledButtonGroup, CAPTION_STR+'1', 0,
    BOOTSTRAP_FAMILY, btn_primary, BOOTSTRAP_OUTLINE);

  AddStyledButtonToButtonGroup(FStyledButtonGroup, CAPTION_STR+'2', 5,
    BOOTSTRAP_FAMILY, btn_secondary, BOOTSTRAP_OUTLINE);

  AddStyledButtonToButtonGroup(FStyledButtonGroup, CAPTION_STR+'3', 8,
    BOOTSTRAP_FAMILY, btn_success, BOOTSTRAP_OUTLINE);

  AddStyledButtonToButtonGroup(FStyledButtonGroup, CAPTION_STR+'4', 10,
    BOOTSTRAP_FAMILY, btn_danger, BOOTSTRAP_OUTLINE);

  tbWidth.Position := BUTTON_WIDTH;
  tbHeight.Position := BUTTON_HEIGHT;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  BuildStyleList;
  BuildCaptionAligmentList;
  BuildImageAlignment;
  ShowCaptionCheckBox.Checked := BUTTONGROUP_SHOW_CAPTIONS;
end;

procedure TfmMain.PopUpMenuClick(Sender: TObject);
begin
  ShowMessage((Sender as TMenuItem).Caption);
end;

procedure TfmMain.ButtonGroupButtonClicked(Sender: TObject; Index: Integer);
begin
  if Sender is TGrpButtonItem then
    ShowMessage(TGrpButtonItem(Sender).Caption);
end;

procedure TfmMain.UpdateButtonGroups(Sender: TObject);
begin
  if Assigned(FStyledButtonGroup) then
  begin
    UpdateShowCaptions(FStyledButtonGroup, ShowCaptionCheckBox.Checked);
    UpdateSetFullSize(FStyledButtonGroup, FullSizeCheckBox.Checked);
    //UpdateSetGroupStyle(FStyledButtonGroup, GroupStyleCheckBox.Checked);
    UpdateShowIcon(FStyledButtonGroup, ShowIconCheckBox.Checked);

    FStyledButtonGroup.ButtonWidth := tbWidth.Position;
    FStyledButtonGroup.ButtonHeight := tbHeight.Position;
  end;
  if Assigned(FButtonGroup) then
  begin
    UpdateShowCaptions(FButtonGroup, ShowCaptionCheckBox.Checked);
    UpdateSetFullSize(FButtonGroup, FullSizeCheckBox.Checked);
    //UpdateSetGroupStyle(FButtonGroup, GroupStyleCheckBox.Checked);
    UpdateShowIcon(FButtonGroup, ShowIconCheckBox.Checked);
    FButtonGroup.ButtonWidth := tbWidth.Position;
    FButtonGroup.ButtonHeight := tbHeight.Position;
  end;
  //Update VCL Button Group
  UpdateShowCaptions(ButtonGroup, ShowCaptionCheckBox.Checked);
  UpdateSetFullSize(ButtonGroup, FullSizeCheckBox.Checked);
  //UpdateSetGroupStyle(ButtonGroup, GroupStyleCheckBox.Checked);
  UpdateShowIcon(ButtonGroup, ShowIconCheckBox.Checked);
  ButtonGroup.ButtonWidth := tbWidth.Position;
  ButtonGroup.ButtonHeight := tbHeight.Position;

  //Update "classic" Styled Button Group
  UpdateShowCaptions(StyledButtonGroup, ShowCaptionCheckBox.Checked);
  UpdateSetFullSize(StyledButtonGroup, FullSizeCheckBox.Checked);
  //UpdateSetGroupStyle(StyledButtonGroup, GroupStyleCheckBox.Checked);
  UpdateShowIcon(StyledButtonGroup, ShowIconCheckBox.Checked);
  StyledButtonGroup.ButtonWidth := tbWidth.Position;
  StyledButtonGroup.ButtonHeight := tbHeight.Position;

  //Update "bootstrap" Styled Button Group
  UpdateShowCaptions(StyledButtonGroupBootstrap, ShowCaptionCheckBox.Checked);
  UpdateSetFullSize(StyledButtonGroupBootstrap, FullSizeCheckBox.Checked);
  //UpdateSetGroupStyle(StyledButtonGroupBootstrap, GroupStyleCheckBox.Checked);
  UpdateShowIcon(StyledButtonGroupBootstrap, ShowIconCheckBox.Checked);
  StyledButtonGroupBootstrap.ButtonWidth := tbWidth.Position;
  StyledButtonGroupBootstrap.ButtonHeight := tbHeight.Position;
end;

procedure TfmMain.BuildCaptionAligmentList;
var
  I: TAlignment;
  LAlignment: string;
begin
  cbCaptionAlignment.Items.Clear;
  for I := Low(TAlignment) to High(TAlignment) do
  begin
    LAlignment := GetEnumName(TypeInfo(TAlignment), Ord(I));
    cbCaptionAlignment.Items.Add(LAlignment);
  end;
  cbCaptionAlignment.ItemIndex := 0;
end;

procedure TfmMain.BuildImageAlignment;
var
  I: TImageAlignment;
  LImageAlignment: string;
begin
  cbImageAlignment.Items.Clear;
  for I := Low(TImageAlignment) to High(TImageAlignment) do
  begin
    LImageAlignment := GetEnumName(TypeInfo(TImageAlignment), Ord(I));
    cbImageAlignment.Items.Add(LImageAlignment);
  end;
  cbImageAlignment.ItemIndex := 0;
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

procedure TfmMain.cbCaptionAlignmentSelect(Sender: TObject);
var
  LCaptionAlignment: TAlignment;
begin
  LCaptionAlignment := TAlignment(cbCaptionAlignment.ItemIndex);
  StyledButtonGroup.CaptionAlignment := LCaptionAlignment;
  StyledButtonGroupBootstrap.CaptionAlignment := LCaptionAlignment;
  FStyledButtonGroup.CaptionAlignment := LCaptionAlignment;
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

procedure TfmMain.cbImageAlignmentSelect(Sender: TObject);
var
  LImageAlignment: TImageAlignment;
begin
  LImageAlignment := TImageAlignment(cbImageAlignment.ItemIndex);
  StyledButtonGroup.ImageAlignment := LImageAlignment;
  StyledButtonGroupBootstrap.ImageAlignment := LImageAlignment;
  FStyledButtonGroup.ImageAlignment := LImageAlignment;
end;

initialization
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

end.