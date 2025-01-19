{******************************************************************************}
{                                                                              }
{  StyledRoundedCorners Demo                                                   }
{  Using StyledRoundedCorners options for Styled Components                    }
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
unit RoundedCornersForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ButtonStylesAttributes,
  Vcl.StandardButtonStyles, Vcl.AngularButtonStyles, Vcl.BootstrapButtonStyles, Vcl.ColorButtonStyles,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ToolWin, Vcl.ActnMan,
  Vcl.ActnCtrls, Vcl.ImgList, Vcl.StdCtrls, Vcl.ButtonGroup, Vcl.Menus,
  DResources, System.Actions, Vcl.ActnList, Vcl.StdActns,
  Vcl.StyledButton, Vcl.StyledToolbar, Vcl.StyledButtonGroup, Vcl.CategoryButtons, Vcl.StyledCategoryButtons,
  System.ImageList, Vcl.VirtualImageList;

type
  TfmRoundedCorners = class(TForm)
    VirtualImageList: TVirtualImageList;
    BottomPanel: TPanel;
    OptionsPanel: TPanel;
    ActionList: TActionList;
    acFileOpen: TFileOpen;
    ButtonGroup: TStyledButtonGroup;
    ShowCaptionCheckBox: TCheckBox;
    ShowIconCheckBox: TCheckBox;
    cbCaptionAlignment: TComboBox;
    cbImageAlignment: TComboBox;
    Toolbar: TStyledToolbar;
    tlbSeparator: TStyledToolButton;
    tlbHome: TStyledToolButton;
    tlbLeft: TStyledToolButton;
    tlbCenter: TStyledToolButton;
    tlbRight: TStyledToolButton;
    CategoryButtons: TStyledCategoryButtons;
    WidthLabel: TLabel;
    tbWidth: TTrackBar;
    HeightLabel: TLabel;
    tbHeight: TTrackBar;
    CenterPanel: TPanel;
    TopPanel: TPanel;
    AngularButton: TStyledButton;
    BootstrapButton: TStyledButton;
    ColoredButton: TStyledButton;
    ClassicButton: TStyledButton;
    GraphicPanel: TPanel;
    GraphicAngularButton: TStyledGraphicButton;
    GraphicBootstrapButton: TStyledGraphicButton;
    GraphicClassicButton: TStyledGraphicButton;
    GraphicColoredButton: TStyledGraphicButton;
    GroupBox2: TGroupBox;
    btn_FABTrash: TStyledButton;
    btn_FABBookmark: TStyledButton;
    btn_FABHome: TStyledButton;
    IconButtonsGroupBox: TGroupBox;
    btn_IconHome: TStyledButton;
    btn_IconDots: TStyledButton;
    btn_IconMenu: TStyledButton;
    btn_trash: TStyledButton;
    btn_IconLaunchDisabled: TStyledButton;
    btn_IconHeart: TStyledButton;
    btn_FABHeartDisabled: TStyledButton;
    procedure FormCreate(Sender: TObject);
    procedure cbCaptionAlignmentSelect(Sender: TObject);
    procedure cbImageAlignmentSelect(Sender: TObject);
    procedure ShowCaptionCheckBoxClick(Sender: TObject);
    procedure ShowIconCheckBoxClick(Sender: TObject);
    procedure tbWidthChange(Sender: TObject);
    procedure tbHeightChange(Sender: TObject);
  private
    procedure BuildImageAlignment;
    procedure BuildCaptionAligmentList;
  public
  end;

var
  fmRoundedCorners: TfmRoundedCorners;

implementation

{$R *.dfm}

uses
  System.TypInfo;

procedure TfmRoundedCorners.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  BuildCaptionAligmentList;
  BuildImageAlignment;
  ShowCaptionCheckBox.Checked := True;
end;

procedure TfmRoundedCorners.ShowCaptionCheckBoxClick(Sender: TObject);
var
  LShowCaption: Boolean;
begin
  LShowCaption := ShowCaptionCheckBox.Checked;
  Toolbar.ShowCaptions := LShowCaption;
  AngularButton.ShowCaption := LShowCaption;
  BootstrapButton.ShowCaption := LShowCaption;
  ColoredButton.ShowCaption := LShowCaption;
  ClassicButton.ShowCaption := LShowCaption;
  GraphicAngularButton.ShowCaption := LShowCaption;
  GraphicBootstrapButton.ShowCaption := LShowCaption;
  GraphicClassicButton.ShowCaption := LShowCaption;
  GraphicColoredButton.ShowCaption := LShowCaption;

  if ShowCaptionCheckBox.Checked then
  begin
    ButtonGroup.ButtonOptions := ButtonGroup.ButtonOptions + [gboShowCaptions];
    CategoryButtons.ButtonOptions := CategoryButtons.ButtonOptions + [boShowCaptions];
  end
  else
  begin
    ButtonGroup.ButtonOptions := ButtonGroup.ButtonOptions - [gboShowCaptions];
    CategoryButtons.ButtonOptions := CategoryButtons.ButtonOptions - [boShowCaptions];
  end;
end;

procedure TfmRoundedCorners.ShowIconCheckBoxClick(Sender: TObject);
begin
  if ShowIconCheckBox.Checked then
  begin
    ButtonGroup.Images := VirtualImageList;
    CategoryButtons.Images := VirtualImageList;
    Toolbar.Images := VirtualImageList;
    AngularButton.Images := VirtualImageList;
    BootstrapButton.Images := VirtualImageList;
    ColoredButton.Images := VirtualImageList;
    ClassicButton.Images := VirtualImageList;
    GraphicAngularButton.Images := VirtualImageList;
    GraphicBootstrapButton.Images := VirtualImageList;
    GraphicClassicButton.Images := VirtualImageList;
    GraphicColoredButton.Images := VirtualImageList;
  end
  else
  begin
    ButtonGroup.Images := nil;
    CategoryButtons.Images := nil;
    Toolbar.Images := nil;
    AngularButton.Images := nil;
    BootstrapButton.Images := nil;
    ColoredButton.Images := nil;
    ClassicButton.Images := nil;
    GraphicAngularButton.Images := nil;
    GraphicBootstrapButton.Images := nil;
    GraphicClassicButton.Images := nil;
    GraphicColoredButton.Images := nil;
  end;
end;

procedure TfmRoundedCorners.tbHeightChange(Sender: TObject);
begin
  Toolbar.ButtonHeight := tbHeight.Position;
  ButtonGroup.ButtonHeight := tbHeight.Position;
  CategoryButtons.ButtonHeight := tbHeight.Position;
end;

procedure TfmRoundedCorners.tbWidthChange(Sender: TObject);
begin
  Toolbar.ButtonWidth := tbWidth.Position;
  ButtonGroup.Width := tbWidth.Position;
  CategoryButtons.Width := tbWidth.Position;
end;

procedure TfmRoundedCorners.BuildCaptionAligmentList;
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

procedure TfmRoundedCorners.BuildImageAlignment;
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

procedure TfmRoundedCorners.cbCaptionAlignmentSelect(Sender: TObject);
var
  LCaptionAlignment: TAlignment;
begin
  LCaptionAlignment := TAlignment(cbCaptionAlignment.ItemIndex);
  ButtonGroup.CaptionAlignment := LCaptionAlignment;
  CategoryButtons.CaptionAlignment := LCaptionAlignment;
  //Toolbar.CaptionAlignment := LCaptionAlignment;
end;

procedure TfmRoundedCorners.cbImageAlignmentSelect(Sender: TObject);
var
  LImageAlignment: TImageAlignment;
begin
  LImageAlignment := TImageAlignment(cbImageAlignment.ItemIndex);
  ButtonGroup.ImageAlignment := LImageAlignment;
  CategoryButtons.ImageAlignment := LImageAlignment;
  tlbHome.ImageAlignment := LImageAlignment;
  tlbLeft.ImageAlignment := LImageAlignment;
  tlbCenter.ImageAlignment := LImageAlignment;
  tlbRight.ImageAlignment := LImageAlignment;
end;

end.
