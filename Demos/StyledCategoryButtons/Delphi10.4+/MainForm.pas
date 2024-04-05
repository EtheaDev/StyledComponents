{******************************************************************************}
{                                                                              }
{  TStyledCategoryButtons Demo                                                 }
{  Comparison TStyledCategoryButtons with TCategoryButtons                     }
{                                                                              }
{  Copyright (c) 2022-2024 (Ethea S.r.l.)                                      }
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
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  DResources, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.CategoryButtons, System.ImageList,
  Vcl.StandardButtonStyles, Vcl.AngularButtonStyles, Vcl.BootstrapButtonStyles, Vcl.ColorButtonStyles,
  Vcl.ImgList, Vcl.VirtualImageList, Vcl.StyledCategoryButtons,
  Vcl.ButtonStylesAttributes, System.Actions, Vcl.ActnList, Vcl.StdActns,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.StyledButton, Vcl.ExtCtrls;
const
  //Params to check
  CAPTION_STR = 'Caption';
  BUTTON_WIDTH = 120;
  BUTTON_HEIGHT = 40;
  CATEGORYBUTTONS_FULLSIZE = True;
  CATEGORYBUTTONS_SHOW_CAPTIONS = True;
  CATEGORYBUTTONS_STYLE = False;

type
  TfmMain = class(TForm)
    VirtualImageList: TVirtualImageList;
    BottomPanel: TPanel;
    LeftPanel: TPanel;
    CreateButton: TStyledButton;
    BottomClientPanel: TPanel;
    WidthLabel: TLabel;
    HeightLabel: TLabel;
    tbWidth: TTrackBar;
    tbHeight: TTrackBar;
    ShowCaptionCheckBox: TCheckBox;
    FullSizeCheckBox: TCheckBox;
    Panel1: TPanel;
    cbChangeStyle: TComboBox;
    StyleLabel: TLabel;
    ShowIconCheckBox: TCheckBox;
    CategoryButtons: TCategoryButtons;
    ActionList: TActionList;
    acFileOpen: TFileOpen;
    StyledCategoryButtons: TStyledCategoryButtons;
    cbCaptionAlignment: TComboBox;
    cbImageAlignment: TComboBox;
    StyledCategoryButtonsBootstrap: TStyledCategoryButtons;
    procedure FormCreate(Sender: TObject);
    procedure CreateButtonClick(Sender: TObject);
    procedure UpdateCategoryButtons(Sender: TObject);
    procedure cbChangeStyleSelect(Sender: TObject);
    procedure CategoryButtonsButtonClicked(Sender: TObject; const Button: TButtonItem);
    procedure cbCaptionAlignmentSelect(Sender: TObject);
    procedure cbImageAlignmentSelect(Sender: TObject);
  private
    FCategoryButtons: TCategoryButtons;
    FStyledCategoryButtons: TStyledCategoryButtons;
    procedure CreateStyledCategoryButtons;
    procedure CreateCategoryButtons;
    procedure CreateCategoriesButtons;
    function AddStyledButtonToCategoryButtons(
      var ACategory: TStyledButtonCategory;
      const ACaption: string; const AImageIndex: Integer = 0;
      const AFamily: TStyledButtonFamily = '';
      const AClass: TStyledButtonClass = '';
      const AAppearance: TStyledButtonAppearance = ''): TStyledButtonItem;
    function AddButtonToCategoryButtons(var ACategory: TButtonCategory;
      const ACaption: string; const AImageIndex: Integer): TButtonItem;
    procedure BuildStyleList;
    procedure BuildImageAlignment;
    procedure BuildCaptionAligmentList;
    procedure UpdateShowCaptions(const ACategoryButtons: TCategoryButtons;
      const AShow: Boolean);
    procedure UpdateSetFullSize(const ACategoryButtons: TCategoryButtons;
      const AFullSize: Boolean);
    procedure UpdateShowIcon(const ACategoryButtons: TCategoryButtons;
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

function TfmMain.AddStyledButtonToCategoryButtons(
  var ACategory: TStyledButtonCategory;
  const ACaption: string; const AImageIndex: Integer = 0;
  const AFamily: TStyledButtonFamily = '';
  const AClass: TStyledButtonClass = '';
  const AAppearance: TStyledButtonAppearance = ''): TStyledButtonItem;
begin
  Result := ACategory.Items.Add as TStyledButtonItem;
  Result.Caption := ACaption;
  Result.Hint := Format('Hint of %s button.',[ACaption]);
  Result.ImageIndex := AImageIndex;
  Result.SetButtonStyle(AFamily, AClass, AAppearance);
end;

function TfmMain.AddButtonToCategoryButtons(
  var ACategory: TButtonCategory;
  const ACaption: string; const AImageIndex: Integer): TButtonItem;
begin
  Result := ACategory.Items.Add;
  Result.Caption := ACaption;
  Result.Hint := Format('Hint of %s button.',[ACaption]);
  Result.ImageIndex := AImageIndex;
end;

procedure TfmMain.CreateButtonClick(Sender: TObject);
begin
  CreateCategoriesButtons;
  LeftPanel.Visible := False;
  BottomClientPanel.Visible := True;
end;

procedure TfmMain.UpdateShowCaptions(const ACategoryButtons: TCategoryButtons;
  const AShow: Boolean);
begin
  if AShow then
    ACategoryButtons.ButtonOptions := ACategoryButtons.ButtonOptions + [boShowCaptions]
  else
    ACategoryButtons.ButtonOptions := ACategoryButtons.ButtonOptions - [boShowCaptions];
end;

procedure TfmMain.UpdateShowIcon(const ACategoryButtons: TCategoryButtons;
  const AShowIcon: Boolean);
begin
  if AShowIcon then
    ACategoryButtons.Images := VirtualImageList
  else
    ACategoryButtons.Images := nil;
end;

procedure TfmMain.UpdateSetFullSize(const ACategoryButtons: TCategoryButtons;
  const AFullSize: Boolean);
begin
  if AFullSize then
    ACategoryButtons.ButtonOptions := ACategoryButtons.ButtonOptions + [boFullSize]
  else
    ACategoryButtons.ButtonOptions := ACategoryButtons.ButtonOptions - [boFullSize];
end;

procedure TfmMain.CreateStyledCategoryButtons;
begin
  FStyledCategoryButtons := TStyledCategoryButtons.Create(Self);
  FStyledCategoryButtons.Parent := Self;
  FStyledCategoryButtons.Align := alLeft;
  FStyledCategoryButtons.Width := Round(BUTTON_WIDTH * 1.5);
  UpdateShowCaptions(FStyledCategoryButtons, CATEGORYBUTTONS_SHOW_CAPTIONS);
  UpdateSetFullSize(FStyledCategoryButtons, CATEGORYBUTTONS_FULLSIZE);
  FStyledCategoryButtons.Images := VirtualImageList;
  FStyledCategoryButtons.ButtonWidth := BUTTON_WIDTH;
  FStyledCategoryButtons.ButtonHeight := BUTTON_HEIGHT;
  FStyledCategoryButtons.ButtonOptions := [boFullSize,boGradientFill,boShowCaptions];
end;

procedure TfmMain.CreateCategoryButtons;
begin
  FCategoryButtons := TCategoryButtons.Create(Self);
  FCategoryButtons.Parent := Self;
  FCategoryButtons.Align := alLeft;
  FCategoryButtons.Width := Round(BUTTON_WIDTH * 1.5);
  UpdateShowCaptions(FCategoryButtons, CategoryButtons_SHOW_CAPTIONS);
  UpdateSetFullSize(FCategoryButtons, CategoryButtons_FULLSIZE);
  FCategoryButtons.Images := VirtualImageList;
  FCategoryButtons.ButtonWidth := BUTTON_WIDTH;
  FCategoryButtons.ButtonHeight := BUTTON_HEIGHT;
  FCategoryButtons.ButtonOptions := [boFullSize,boGradientFill,boShowCaptions];
end;

procedure TfmMain.CreateCategoriesButtons;
var
  LCategory: TButtonCategory;
  LStyledCategory: TStyledButtonCategory;
begin
  //Create Standard CategoryButtons
  CreateCategoryButtons;
  //Create first Category
  LCategory := FCategoryButtons.Categories.Add;
  LCategory.Caption := 'Category 1';
  //Add Buttons to first Category
  AddButtonToCategoryButtons(LCategory, CAPTION_STR+'1', 0);
  AddButtonToCategoryButtons(LCategory, CAPTION_STR+'2', 5);
  //Create second Category
  LCategory := FCategoryButtons.Categories.Add;
  LCategory.Caption := 'Category 2';
  //Add Buttons to second Category
  AddButtonToCategoryButtons(LCategory, CAPTION_STR+'3', 8);
  AddButtonToCategoryButtons(LCategory, CAPTION_STR+'4', 10);

  //Create Styled CategoryButtons
  CreateStyledCategoryButtons;
  //Create first Category
  LStyledCategory := FStyledCategoryButtons.Categories.Add;
  LStyledCategory.Caption := 'Category 1';
  //Add Buttons to first Category
  AddStyledButtonToCategoryButtons(LStyledCategory, CAPTION_STR+'1', 0,
    BOOTSTRAP_FAMILY, btn_primary, BOOTSTRAP_OUTLINE);
  AddStyledButtonToCategoryButtons(LStyledCategory, CAPTION_STR+'2', 5,
    BOOTSTRAP_FAMILY, btn_secondary, BOOTSTRAP_OUTLINE);
  //Create second Category
  LStyledCategory := FStyledCategoryButtons.Categories.Add;
  LStyledCategory.Caption := 'Category 2';
  //Add Buttons to second Category
  AddStyledButtonToCategoryButtons(LStyledCategory, CAPTION_STR+'3', 8,
    BOOTSTRAP_FAMILY, btn_success, BOOTSTRAP_OUTLINE);
  AddStyledButtonToCategoryButtons(LStyledCategory, CAPTION_STR+'4', 10,
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
  ShowCaptionCheckBox.Checked := CategoryButtons_SHOW_CAPTIONS;
end;

procedure TfmMain.CategoryButtonsButtonClicked(Sender: TObject;
  const Button: TButtonItem);
begin
  if Assigned(Button) then
    ShowMessage(Button.Caption);
end;

procedure TfmMain.UpdateCategoryButtons(Sender: TObject);
begin
  if Assigned(FStyledCategoryButtons) then
  begin
    UpdateShowCaptions(FStyledCategoryButtons, ShowCaptionCheckBox.Checked);
    UpdateSetFullSize(FStyledCategoryButtons, FullSizeCheckBox.Checked);
    UpdateShowIcon(FStyledCategoryButtons, ShowIconCheckBox.Checked);

    FStyledCategoryButtons.ButtonWidth := tbWidth.Position;
    FStyledCategoryButtons.ButtonHeight := tbHeight.Position;
  end;
  if Assigned(FCategoryButtons) then
  begin
    UpdateShowCaptions(FCategoryButtons, ShowCaptionCheckBox.Checked);
    UpdateSetFullSize(FCategoryButtons, FullSizeCheckBox.Checked);
    UpdateShowIcon(FCategoryButtons, ShowIconCheckBox.Checked);
    FCategoryButtons.ButtonWidth := tbWidth.Position;
    FCategoryButtons.ButtonHeight := tbHeight.Position;
  end;
  //Update VCL Button Group
  UpdateShowCaptions(CategoryButtons, ShowCaptionCheckBox.Checked);
  UpdateSetFullSize(CategoryButtons, FullSizeCheckBox.Checked);
  UpdateShowIcon(CategoryButtons, ShowIconCheckBox.Checked);
  CategoryButtons.ButtonWidth := tbWidth.Position;
  CategoryButtons.ButtonHeight := tbHeight.Position;

  //Update "classic" Styled Button Group
  UpdateShowCaptions(StyledCategoryButtons, ShowCaptionCheckBox.Checked);
  UpdateSetFullSize(StyledCategoryButtons, FullSizeCheckBox.Checked);
  UpdateShowIcon(StyledCategoryButtons, ShowIconCheckBox.Checked);
  StyledCategoryButtons.ButtonWidth := tbWidth.Position;
  StyledCategoryButtons.ButtonHeight := tbHeight.Position;

  //Update "bootstrap" Styled Button Group
  UpdateShowCaptions(StyledCategoryButtonsBootstrap, ShowCaptionCheckBox.Checked);
  UpdateSetFullSize(StyledCategoryButtonsBootstrap, FullSizeCheckBox.Checked);
  UpdateShowIcon(StyledCategoryButtonsBootstrap, ShowIconCheckBox.Checked);
  StyledCategoryButtonsBootstrap.ButtonWidth := tbWidth.Position;
  StyledCategoryButtonsBootstrap.ButtonHeight := tbHeight.Position;
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
  StyledCategoryButtons.CaptionAlignment := LCaptionAlignment;
  StyledCategoryButtonsBootstrap.CaptionAlignment := LCaptionAlignment;
  FStyledCategoryButtons.CaptionAlignment := LCaptionAlignment;
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
  StyledCategoryButtons.ImageAlignment := LImageAlignment;
  StyledCategoryButtonsBootstrap.ImageAlignment := LImageAlignment;
  FStyledCategoryButtons.ImageAlignment := LImageAlignment;
end;

initialization
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

end.
