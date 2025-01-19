{******************************************************************************}
{                                                                              }
{  TStyledButtonGroup Demo                                                     }
{  Comparison TStyledButtonGroup with TButtonGroup                             }
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
unit StyledButtonGroupFormOld;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ButtonStylesAttributes,
  Vcl.StandardButtonStyles, Vcl.AngularButtonStyles, Vcl.BootstrapButtonStyles, Vcl.ColorButtonStyles,
  Vcl.StyledButton, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ToolWin, Vcl.ActnMan,
  Vcl.ActnCtrls, Vcl.ImgList, Vcl.StdCtrls, Vcl.ButtonGroup, Vcl.StyledButtonGroup, Vcl.Menus,
  System.Actions, Vcl.ActnList, Vcl.StdActns;

const
  //Params to check
  CAPTION_STR = 'Caption';
  BUTTON_WIDTH = 120;
  BUTTON_HEIGHT = 40;
  BUTTONGROUP_FULLSIZE = True;
  BUTTONGROUP_SHOW_CAPTIONS = True;
  BUTTON_GROUP_GROUP_STYLE = False;

type
  TfmStyledButtonGroup = class(TForm)
    ImageList32: TImageList;
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
    ShowIconCheckBox: TCheckBox;
    ButtonGroup: TButtonGroup;
    ActionList: TActionList;
    acFileOpen: TFileOpen;
    StyledButtonGroup: TStyledButtonGroup;
    StyledButtonGroupBootstrap: TStyledButtonGroup;
    cbCaptionAlignment: TComboBox;
    cbImageAlignment: TComboBox;
    BadgeTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure CreateButtonClick(Sender: TObject);
    procedure UpdateButtonGroups(Sender: TObject);
    procedure ButtonGroupButtonClicked(Sender: TObject; Index: Integer);
    procedure cbCaptionAlignmentSelect(Sender: TObject);
    procedure cbImageAlignmentSelect(Sender: TObject);
    procedure StyledButtonGroupBootstrapGetNotificationBadgeInfo(
      const AButtonItemIndex: Integer; var ABadgeContent: string;
      var ASize: TNotificationBadgeSize;
      var APosition: TNotificationBadgePosition; var AColor, AFontColor: TColor;
      var AFontStyle: TFontStyles);
    procedure BadgeTimerTimer(Sender: TObject);
  private
    FButtonGroup: TButtonGroup;
    FStyledButtonGroup: TStyledButtonGroup;
    FNotificationCount: Integer;
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
  fmStyledButtonGroup: TfmStyledButtonGroup;

implementation

{$R *.dfm}

uses
  System.TypInfo,
  Vcl.StyledTaskDialog,
  Vcl.StyledButtonEditorUnit;

function TfmStyledButtonGroup.AddStyledButtonToButtonGroup(
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

function TfmStyledButtonGroup.AddButtonToButtonGroup(var AButtonGroup: TButtonGroup;
  const ACaption: string; const AImageIndex: Integer): TGrpButtonItem;
begin
  Result := AButtonGroup.Items.Add;
  Result.Caption := ACaption;
  Result.Hint := Format('Hint of %s button.',[ACaption]);
  Result.ImageIndex := AImageIndex;
end;

procedure TfmStyledButtonGroup.CreateButtonClick(Sender: TObject);
begin
  CreateButtonGroups;
  LeftPanel.Visible := False;
  BottomClientPanel.Visible := True;
end;

procedure TfmStyledButtonGroup.UpdateShowCaptions(const AButtonGroup: TButtonGroup;
  const AShow: Boolean);
begin
  if AShow then
    AButtonGroup.ButtonOptions := AButtonGroup.ButtonOptions + [gboShowCaptions]
  else
    AButtonGroup.ButtonOptions := AButtonGroup.ButtonOptions - [gboShowCaptions];
end;

procedure TfmStyledButtonGroup.UpdateShowIcon(const AButtonGroup: TButtonGroup;
  const AShowIcon: Boolean);
begin
  if AShowIcon then
    AButtonGroup.Images := ImageList32
  else
    AButtonGroup.Images := nil;
end;

procedure TfmStyledButtonGroup.UpdateSetFullSize(const AButtonGroup: TButtonGroup;
  const AFullSize: Boolean);
begin
  if AFullSize then
    AButtonGroup.ButtonOptions := AButtonGroup.ButtonOptions + [gboFullSize]
  else
    AButtonGroup.ButtonOptions := AButtonGroup.ButtonOptions - [gboFullSize];
end;

procedure TfmStyledButtonGroup.UpdateSetGroupStyle(const AButtonGroup: TButtonGroup;
  const AGroupStyle: Boolean);
begin
  if AGroupStyle then
    AButtonGroup.ButtonOptions := AButtonGroup.ButtonOptions + [gboGroupStyle]
  else
    AButtonGroup.ButtonOptions := AButtonGroup.ButtonOptions - [gboGroupStyle];
end;

procedure TfmStyledButtonGroup.CreateStyledButtonGroup;
begin
  FStyledButtonGroup := TStyledButtonGroup.Create(Self);
  FStyledButtonGroup.Parent := Self;
  FStyledButtonGroup.Align := alLeft;
  FStyledButtonGroup.Width := Round(BUTTON_WIDTH * 1.5);
  UpdateShowCaptions(FStyledButtonGroup, BUTTONGROUP_SHOW_CAPTIONS);
  UpdateSetFullSize(FStyledButtonGroup, BUTTONGROUP_FULLSIZE);
  UpdateSetGroupStyle(FStyledButtonGroup, BUTTON_GROUP_GROUP_STYLE);
  FStyledButtonGroup.Images := ImageList32;
  FStyledButtonGroup.ButtonWidth := BUTTON_WIDTH;
  FStyledButtonGroup.ButtonHeight := BUTTON_HEIGHT;
  FStyledButtonGroup.OnButtonClicked := ButtonGroupButtonClicked;
end;

procedure TfmStyledButtonGroup.CreateButtonGroup;
begin
  FButtonGroup := TButtonGroup.Create(Self);
  FButtonGroup.Parent := Self;
  FButtonGroup.Align := alLeft;
  FButtonGroup.Width := Round(BUTTON_WIDTH * 1.5);
  UpdateShowCaptions(FButtonGroup, BUTTONGROUP_SHOW_CAPTIONS);
  UpdateSetFullSize(FButtonGroup, BUTTONGROUP_FULLSIZE);
  UpdateSetGroupStyle(FButtonGroup, BUTTON_GROUP_GROUP_STYLE);
  FButtonGroup.Images := ImageList32;
  FButtonGroup.ButtonWidth := BUTTON_WIDTH;
  FButtonGroup.ButtonHeight := BUTTON_HEIGHT;
  FButtonGroup.OnButtonClicked := ButtonGroupButtonClicked;
end;

procedure TfmStyledButtonGroup.CreateButtonGroups;
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

procedure TfmStyledButtonGroup.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  BuildCaptionAligmentList;
  BuildImageAlignment;
  ShowCaptionCheckBox.Checked := BUTTONGROUP_SHOW_CAPTIONS;
end;

procedure TfmStyledButtonGroup.BadgeTimerTimer(Sender: TObject);
begin
  Inc(FNotificationCount);
  //force repaint of Notificationbadges
  StyledButtonGroupBootstrap.Repaint;
end;

procedure TfmStyledButtonGroup.StyledButtonGroupBootstrapGetNotificationBadgeInfo(
  const AButtonItemIndex: Integer; var ABadgeContent: string;
  var ASize: TNotificationBadgeSize; var APosition: TNotificationBadgePosition;
  var AColor, AFontColor: TColor; var AFontStyle: TFontStyles);
begin
  if (AButtonItemIndex = 0) then
    ABadgeContent := IntToStr(FNotificationCount+10)
  else if (AButtonItemIndex = 1) then
  begin
    AColor := clBlue;
    ABadgeContent := IntToStr(FNotificationCount+20);
  end;
end;

procedure TfmStyledButtonGroup.ButtonGroupButtonClicked(Sender: TObject; Index: Integer);
begin
  if Sender is TButtonGroup then
  begin
    if TButtonGroup(Sender).Items[Index] is TGrpButtonItem then
      StyledShowMessage(TGrpButtonItem(TButtonGroup(Sender).Items[Index]).Caption);
  end;
end;

procedure TfmStyledButtonGroup.UpdateButtonGroups(Sender: TObject);
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

procedure TfmStyledButtonGroup.BuildCaptionAligmentList;
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

procedure TfmStyledButtonGroup.BuildImageAlignment;
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

procedure TfmStyledButtonGroup.cbCaptionAlignmentSelect(Sender: TObject);
var
  LCaptionAlignment: TAlignment;
begin
  LCaptionAlignment := TAlignment(cbCaptionAlignment.ItemIndex);
  StyledButtonGroup.CaptionAlignment := LCaptionAlignment;
  StyledButtonGroupBootstrap.CaptionAlignment := LCaptionAlignment;
  FStyledButtonGroup.CaptionAlignment := LCaptionAlignment;
end;

procedure TfmStyledButtonGroup.cbImageAlignmentSelect(Sender: TObject);
var
  LImageAlignment: TImageAlignment;
begin
  LImageAlignment := TImageAlignment(cbImageAlignment.ItemIndex);
  StyledButtonGroup.ImageAlignment := LImageAlignment;
  StyledButtonGroupBootstrap.ImageAlignment := LImageAlignment;
  FStyledButtonGroup.ImageAlignment := LImageAlignment;
end;

end.
