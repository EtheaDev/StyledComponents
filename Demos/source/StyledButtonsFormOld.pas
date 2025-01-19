{******************************************************************************}
{                                                                              }
{  StyledButtonsDemo: a Demo to show StyledButtons                             }
{  with different Familes (Classic, Bootstrap and Angular                      }
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
unit StyledButtonsFormOld;

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
  Vcl.ImgList,
  Vcl.StyledButton,
  System.Actions,
  Vcl.ActnList,
  Vcl.ButtonStylesAttributes,
  Vcl.StyledButtonEditorUnit,
  Vcl.Menus,
  Vcl.ComCtrls,
  Vcl.Buttons;

type
  TfmStyledButtons = class(TForm)
    ActionList: TActionList;
    TestAction: TAction;
    PopupMenu: TPopupMenu;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Exit1: TMenuItem;
    Panel1: TPanel;
    ShowEditButton: TStyledButton;
    StyledButtonCircular: TStyledButton;
    PageControl: TPageControl;
    tsBootstrap: TTabSheet;
    tsAngular: TTabSheet;
    tsClassic: TTabSheet;
    BootStrapLinkLabel: TLinkLabel;
    gbBootstrapNormal: TGroupBox;
    btn_Primary: TStyledButton;
    btn_Secondary: TStyledButton;
    btn_Success: TStyledButton;
    btn_Danger: TStyledButton;
    btn_Warning: TStyledButton;
    btn_Info: TStyledButton;
    btn_Light: TStyledButton;
    btn_Dark: TStyledButton;
    gbBootstrapOutlined: TGroupBox;
    btn_OutlinePrimary: TStyledButton;
    btn_OutlineSecondary: TStyledButton;
    btn_OutlineSuccess: TStyledButton;
    btn_OutlineDanger: TStyledButton;
    btn_OutlineWarning: TStyledButton;
    btn_OutlineInfo: TStyledButton;
    btn_OutlineLight: TStyledButton;
    btn_OutlineDark: TStyledButton;
    gbBuutstrapDisabled: TGroupBox;
    btn_DisabledPrimary: TStyledButton;
    btn_DisabledSecondary: TStyledButton;
    btn_DisabledSuccess: TStyledButton;
    btn_DisabledDanger: TStyledButton;
    btn_DisabledWarning: TStyledButton;
    btn_DisabledInfo: TStyledButton;
    btn_DisabledLight: TStyledButton;
    btn_DisabledDark: TStyledButton;
    gbAngularRaised: TGroupBox;
    btn_BasicBasic: TStyledButton;
    btn_BasicPrimary: TStyledButton;
    btn_BasicAccent: TStyledButton;
    btn_BasicWarn: TStyledButton;
    btn_BasicDisabled: TStyledButton;
    btn_RaisedBasic: TStyledButton;
    btn_RaisedPrimary: TStyledButton;
    btn_RaisedAccent: TStyledButton;
    btn_RaisedWarn: TStyledButton;
    btn_RaisedDisabled: TStyledButton;
    AngularThemesPanel: TPanel;
    rgAngularLightThemes: TRadioGroup;
    rgAngularDarkThemes: TRadioGroup;
    gpAngularStroked: TGroupBox;
    btn_StrokedBasic: TStyledButton;
    btn_StrokedPrimary: TStyledButton;
    btn_StrokedWarn: TStyledButton;
    btn_StrokedDisabled: TStyledButton;
    btn_StrokedAccent: TStyledButton;
    gbAngularFlat: TGroupBox;
    btn_FlatBasic: TStyledButton;
    btn_FlatPrimary: TStyledButton;
    btn_FlatWarn: TStyledButton;
    btn_FlatDisabled: TStyledButton;
    btn_FlatAccent: TStyledButton;
    IconButtonsGroupBox: TGroupBox;
    btn_IconHome: TStyledButton;
    btn_IconDots: TStyledButton;
    btn_IconMenu: TStyledButton;
    btn_IconHeart: TStyledButton;
    btn_trash: TStyledButton;
    btn_IconLaunchDisabled: TStyledButton;
    GroupBox2: TGroupBox;
    btn_FABTrash: TStyledButton;
    btn_FABBookmark: TStyledButton;
    ImageList32: TImageList;
    btn_FABHome: TStyledButton;
    btn_FABHeartDisabled: TStyledButton;
    StyledButtonSquare: TStyledButton;
    gbAngularModalResult: TGroupBox;
    btn_AngularOK: TStyledButton;
    btn_AngularCancel: TStyledButton;
    btn_AngularAbort: TStyledButton;
    btn_AngularRetry: TStyledButton;
    btn_AngularIgnore: TStyledButton;
    btn_AngularYes: TStyledButton;
    btn_AngularNo: TStyledButton;
    btn_AngularClose: TStyledButton;
    btn_AngularHelp: TStyledButton;
    btn_AngularAll: TStyledButton;
    gbBootstrapModalResult: TGroupBox;
    btn_BootstrapOK: TStyledButton;
    btn_BootstrapCancel: TStyledButton;
    btn_BootstrapAbort: TStyledButton;
    btn_BootstrapRetry: TStyledButton;
    btn_BootstrapIgnore: TStyledButton;
    btn_BootstrapYes: TStyledButton;
    btn_BootstrapNo: TStyledButton;
    btn_BootstrapClose: TStyledButton;
    btn_BootstrapHelp: TStyledButton;
    btn_BootstrapAll: TStyledButton;
    GroupBox3: TGroupBox;
    VCLButton: TButton;
    VCLButtonDisabled: TButton;
    StyledButton: TStyledButton;
    StyledButtonDisable: TStyledButton;
    StyledButtonStyled: TStyledButton;
    VCLButtonStyled: TButton;
    gbClassicModalResult: TGroupBox;
    btn_ClassicOK: TStyledButton;
    btn_ClassicCancel: TStyledButton;
    btn_ClassicAbort: TStyledButton;
    btn_ClassicRetry: TStyledButton;
    btn_ClassicIgnore: TStyledButton;
    btn_ClassicYes: TStyledButton;
    btn_ClassicNo: TStyledButton;
    btn_ClassicClose: TStyledButton;
    btn_ClassicHelp: TStyledButton;
    btn_ClassicAll: TStyledButton;
    DefaultStyledButton: TStyledButton;
    CancelStyledButton: TStyledButton;
    tsBasicColor: TTabSheet;
    tsSVGColor: TTabSheet;
    BasicColorScrollBox: TScrollBox;
    GroupBoxNormal: TGroupBox;
    FlowPanelNormal: TFlowPanel;
    GroupBoxOutline: TGroupBox;
    FlowPanelOutLine: TFlowPanel;
    SvgColorScrollBox: TScrollBox;
    SvgColorNormalGroupBox: TGroupBox;
    SvgColorNormalFlowPanel: TFlowPanel;
    SvgColorOutlineGroupBox: TGroupBox;
    SvgColorOutlineFlowPanel: TFlowPanel;
    ClassicScrollBox: TScrollBox;
    ClassicNormalGroupBox: TGroupBox;
    ClassicNormalFlowPanel: TFlowPanel;
    ClassicOutlineGroupBox: TGroupBox;
    ClassicOutlineFlowPanel: TFlowPanel;
    ButtonSplit: TButton;
    StyledButtonSplit: TStyledButton;
    BadgeTimer: TTimer;
    procedure TestActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LinkLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure rgAngularLightThemesClick(Sender: TObject);
    procedure AngularThemesPanelResize(Sender: TObject);
    procedure rgAngularDarkThemesClick(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure FlowPanelResize(Sender: TObject);
    procedure ScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PopupMenuClick(Sender: TObject);
    procedure StyledButtonSquareClick(Sender: TObject);
    procedure StyledButtonCircularClick(Sender: TObject);
    procedure BadgeTimerTimer(Sender: TObject);
  private
    FNotificationCount: Integer;
    procedure RepaintAngularBtnWithMR(const AFamily: TStyledButtonFamily);
    procedure BuildFamilyPreview(const AFamily: TStyledButtonFamily;
      const AAppearance: TStyledButtonAppearance;
      const AFlowPanel: TFlowPanel);
  protected
  end;

var
  fmStyledButtons: TfmStyledButtons;

implementation

{$R *.dfm}

uses
  System.TypInfo
  , System.Types
  , Vcl.Themes
  , WinApi.ShellAPI
  , Vcl.StandardButtonStyles
  , Vcl.AngularButtonStyles
  , Vcl.BootstrapButtonStyles
  , Vcl.ColorButtonStyles
  , Vcl.StyledTaskDialog
  ;

{ TMainForm }

procedure TfmStyledButtons.ButtonClick(Sender: TObject);
const
  Msg = 'Caption: %s - ModalResult: %d';
begin
  if Sender is TButton then
    StyledShowMessage(Format(Msg,[TButton(Sender).Caption,
      TButton(Sender).ModalResult]))
  else
    StyledShowMessage(Format(Msg,[TStyledButton(Sender).Caption,
      TStyledButton(Sender).ModalResult]));
end;

procedure TfmStyledButtons.FlowPanelResize(Sender: TObject);
begin
  TFlowPanel(Sender).Parent.Height := TFlowPanel(Sender).Height + 20;
end;

procedure TfmStyledButtons.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  BuildFamilyPreview(DEFAULT_CLASSIC_FAMILY, DEFAULT_APPEARANCE, ClassicNormalFlowPanel);
  BuildFamilyPreview(DEFAULT_CLASSIC_FAMILY, OUTLINE_APPEARANCE, ClassicOutlineFlowPanel);
  BuildFamilyPreview(BASIC_COLOR_FAMILY, COLOR_BTN_NORMAL, FlowPanelNormal);
  BuildFamilyPreview(BASIC_COLOR_FAMILY, COLOR_BTN_OUTLINE, FlowPanelOutline);
  BuildFamilyPreview(SVG_COLOR_FAMILY, COLOR_BTN_NORMAL, SvgColorNormalFlowPanel);
  BuildFamilyPreview(SVG_COLOR_FAMILY, COLOR_BTN_OUTLINE, SvgColorOutlineFlowPanel);

  PageControl.ActivePage := tsBootStrap;
end;

procedure TfmStyledButtons.TestActionExecute(Sender: TObject);
begin
  EditStyledButton(ShowEditButton);
  ShowEditButton.Hint := Format('%s/%s/%s',
    [ShowEditButton.StyleFamily,
    ShowEditButton.StyleClass,
    ShowEditButton.StyleAppearance]);
end;

procedure TfmStyledButtons.LinkLabelLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  ShellExecute(handle, 'open',
    PChar(Link), nil, nil, SW_SHOWNORMAL);
end;

procedure TfmStyledButtons.PopupMenuClick(Sender: TObject);
begin
  StyledShowMessage((Sender as TMenuItem).Caption);
end;

procedure TfmStyledButtons.RepaintAngularBtnWithMR(const AFamily: TStyledButtonFamily);
begin
  btn_AngularOK.StyleFamily := AFamily;
  btn_AngularCancel.StyleFamily := AFamily;
  btn_AngularAbort.StyleFamily := AFamily;
  btn_AngularRetry.StyleFamily := AFamily;
  btn_AngularIgnore.StyleFamily := AFamily;
  btn_AngularYes.StyleFamily := AFamily;
  btn_AngularNo.StyleFamily := AFamily;
  btn_AngularClose.StyleFamily := AFamily;
  btn_AngularHelp.StyleFamily := AFamily;
  btn_AngularAll.StyleFamily := AFamily;
end;

procedure TfmStyledButtons.rgAngularDarkThemesClick(Sender: TObject);
begin
  TStyleManager.TrySetStyle('Windows10 SlateGray');
  rgAngularLightThemes.ItemIndex := -1;
  case rgAngularDarkThemes.ItemIndex of
    0: //Pink & Blue-grey
    begin
      btn_BasicPrimary.SetButtonStyle(ANGULAR_DARK_FAMILY, btn_PrimaryPink, BasicAttr);
      btn_RaisedPrimary.SetButtonStyle(ANGULAR_DARK_FAMILY, btn_PrimaryPink, RaisedAttr);
      btn_StrokedPrimary.SetButtonStyle(ANGULAR_DARK_FAMILY, btn_PrimaryPink, StrokedAttr);
      btn_FlatPrimary.SetButtonStyle(ANGULAR_DARK_FAMILY, btn_PrimaryPink, FlatAttr);
      btn_BasicAccent.SetButtonStyle(ANGULAR_DARK_FAMILY, btn_AccentBlueGray, BasicAttr);
      btn_RaisedAccent.SetButtonStyle(ANGULAR_DARK_FAMILY, btn_AccentBlueGray, RaisedAttr);
      btn_StrokedAccent.SetButtonStyle(ANGULAR_DARK_FAMILY, btn_AccentBlueGray, StrokedAttr);
      btn_FlatAccent.SetButtonStyle(ANGULAR_DARK_FAMILY, btn_AccentBlueGray, FlatAttr);
      {$IFDEF D10_4+}
        btn_IconHome.ImageName := 'home-pink';
        btn_IconMenu.ImageName := 'menu-blue-grey';
        btn_FABBookmark.ImageName := 'bookmark-white';
      {$ELSE}
        btn_IconHome.ImageIndex := 2;
        btn_IconMenu.ImageIndex := 6;
        btn_FABBookmark.ImageIndex := 15;
      {$ENDIF}
    end;
    1: //Purple & Green
    begin
      btn_BasicPrimary.SetButtonStyle(ANGULAR_DARK_FAMILY, btn_PrimaryPurple, BasicAttr);
      btn_RaisedPrimary.SetButtonStyle(ANGULAR_DARK_FAMILY, btn_PrimaryPurple, RaisedAttr);
      btn_StrokedPrimary.SetButtonStyle(ANGULAR_DARK_FAMILY, btn_PrimaryPurple, StrokedAttr);
      btn_FlatPrimary.SetButtonStyle(ANGULAR_DARK_FAMILY, btn_PrimaryPurple, FlatAttr);
      btn_BasicAccent.SetButtonStyle(ANGULAR_DARK_FAMILY, btn_AccentGreen, BasicAttr);
      btn_RaisedAccent.SetButtonStyle(ANGULAR_DARK_FAMILY, btn_AccentGreen, RaisedAttr);
      btn_StrokedAccent.SetButtonStyle(ANGULAR_DARK_FAMILY, btn_AccentGreen, StrokedAttr);
      btn_FlatAccent.SetButtonStyle(ANGULAR_DARK_FAMILY, btn_AccentGreen, FlatAttr);
      {$IFDEF D10_4+}
        btn_IconHome.ImageName := 'home-purple';
        btn_IconMenu.ImageName := 'menu-green';
        btn_FABBookmark.ImageName := 'bookmark-black';
      {$ELSE}
        btn_IconHome.ImageIndex := 3;
        btn_IconMenu.ImageIndex := 7;
        btn_FABBookmark.ImageIndex := 16;
      {$ENDIF}
    end;
  end;
  btn_FlatPrimary.AssignStyleTo(btn_FABTrash);
  btn_FlatAccent.AssignStyleTo(btn_FABBookmark);
  RepaintAngularBtnWithMR(ANGULAR_DARK_FAMILY);
end;

procedure TfmStyledButtons.rgAngularLightThemesClick(Sender: TObject);
begin
  TStyleManager.TrySetStyle('Light');
  rgAngularDarkThemes.ItemIndex := -1;
  case rgAngularLightThemes.ItemIndex of
    0: //Deep Purple & Amber
    begin
      btn_BasicPrimary.SetButtonStyle(ANGULAR_LIGHT_FAMILY, btn_PrimaryDeepPurple, BasicAttr);
      btn_RaisedPrimary.SetButtonStyle(ANGULAR_LIGHT_FAMILY, btn_PrimaryDeepPurple, RaisedAttr);
      btn_StrokedPrimary.SetButtonStyle(ANGULAR_LIGHT_FAMILY, btn_PrimaryDeepPurple, StrokedAttr);
      btn_FlatPrimary.SetButtonStyle(ANGULAR_LIGHT_FAMILY, btn_PrimaryDeepPurple, FlatAttr);
      btn_BasicAccent.SetButtonStyle(ANGULAR_LIGHT_FAMILY, btn_AccentAmber, BasicAttr);
      btn_RaisedAccent.SetButtonStyle(ANGULAR_LIGHT_FAMILY, btn_AccentAmber, RaisedAttr);
      btn_StrokedAccent.SetButtonStyle(ANGULAR_LIGHT_FAMILY, btn_AccentAmber, StrokedAttr);
      btn_FlatAccent.SetButtonStyle(ANGULAR_LIGHT_FAMILY, btn_AccentAmber, FlatAttr);
      {$IFDEF D10_4+}
        btn_IconHome.ImageName := 'home-deeppurple';
        btn_IconMenu.ImageName := 'menu-amber';
        btn_FABBookmark.ImageName := 'bookmark-black';
      {$ELSE}
        btn_IconHome.ImageIndex := 0;
        btn_IconMenu.ImageIndex := 4;
        btn_FABBookmark.ImageIndex := 16;
      {$ENDIF}
    end;
    1: //Indigo & Pink
    begin
      btn_BasicPrimary.SetButtonStyle(ANGULAR_LIGHT_FAMILY, btn_PrimaryIndigo, BasicAttr);
      btn_RaisedPrimary.SetButtonStyle(ANGULAR_LIGHT_FAMILY, btn_PrimaryIndigo, RaisedAttr);
      btn_StrokedPrimary.SetButtonStyle(ANGULAR_LIGHT_FAMILY, btn_PrimaryIndigo, StrokedAttr);
      btn_FlatPrimary.SetButtonStyle(ANGULAR_LIGHT_FAMILY, btn_PrimaryIndigo, FlatAttr);
      btn_BasicAccent.SetButtonStyle(ANGULAR_LIGHT_FAMILY, btn_AccentPink, BasicAttr);
      btn_RaisedAccent.SetButtonStyle(ANGULAR_LIGHT_FAMILY, btn_AccentPink, RaisedAttr);
      btn_StrokedAccent.SetButtonStyle(ANGULAR_LIGHT_FAMILY, btn_AccentPink, StrokedAttr);
      btn_FlatAccent.SetButtonStyle(ANGULAR_LIGHT_FAMILY, btn_AccentPink, FlatAttr);
      btn_IconHome.ImageIndex := 1;
      {$IFDEF D10_4+}
        btn_IconHome.ImageName := 'home-indigo';
        btn_IconMenu.ImageName := 'menu-pink';
        btn_FABBookmark.ImageName := 'bookmark-white';
      {$ELSE}
        btn_IconHome.ImageIndex := 0;
        btn_IconMenu.ImageIndex := 5;
        btn_FABBookmark.ImageIndex := 15;
      {$ENDIF}
    end;
  end;
  btn_FlatPrimary.AssignStyleTo(btn_FABTrash);
  btn_FlatAccent.AssignStyleTo(btn_FABBookmark);
  RepaintAngularBtnWithMR(ANGULAR_LIGHT_FAMILY);
end;

procedure TfmStyledButtons.ScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  LTopLeft, LTopRight, LBottomLeft, LBottomRight: SmallInt;
  LPoint: TPoint;
  ScrollBox: TScrollBox;
begin
  ScrollBox := TScrollBox(Sender);
  LPoint := ScrollBox.ClientToScreen(Point(0,0));
  LTopLeft := LPoint.X;
  LTopRight := LTopLeft + ScrollBox.ClientWidth;
  LBottomLeft := LPoint.Y;
  LBottomRight := LBottomLeft + ScrollBox.ClientWidth;
  if (MousePos.X >= LTopLeft) and
    (MousePos.X <= LTopRight) and
    (MousePos.Y >= LBottomLeft) and
    (MousePos.Y <= LBottomRight) then
  begin
    ScrollBox.VertScrollBar.Position := ScrollBox.VertScrollBar.Position - WheelDelta;
    Handled := True;
  end;
end;

procedure TfmStyledButtons.StyledButtonCircularClick(Sender: TObject);
begin
  EditStyledButton(StyledButtonCircular);
end;

procedure TfmStyledButtons.StyledButtonSquareClick(Sender: TObject);
begin
  EditStyledButton(StyledButtonSquare);
end;

procedure TfmStyledButtons.AngularThemesPanelResize(Sender: TObject);
begin
  rgAngularLightThemes.Width := AngularThemesPanel.Width div 2;
end;

procedure TfmStyledButtons.BadgeTimerTimer(Sender: TObject);
begin
  Inc(FNotificationCount);
  btn_IconHome.NotificationBadge.NotificationCount := FNotificationCount;
end;

procedure TfmStyledButtons.BuildFamilyPreview(const AFamily: TStyledButtonFamily;
  const AAppearance: TStyledButtonAppearance;
  const AFlowPanel: TFlowPanel);
var
  J: Integer;
  LClasses: TButtonClasses;
  LDefaultClass: TStyledButtonClass;

  procedure CreateButton(
    const AParent: TFlowPanel;
    const AClass: TStyledButtonClass);
  var
    LStyledButton: TStyledButton;
  begin
    LStyledButton := TStyledButton.CreateStyled(Self,
      AFamily, AClass, AAppearance);
    LStyledButton.Width := BUTTON_WIDTH;
    LStyledButton.Height := BUTTON_HEIGHT;
    LStyledButton.AlignWithMargins := True;
    LStyledButton.Caption := AClass;
    LStyledButton.Hint := AClass;
    LStyledButton.OnClick := ButtonClick;
    LStyledButton.Parent := AParent;
  end;

begin
  if AFlowPanel.ControlCount > 0 then
    Exit;

  Screen.Cursor := crHourGlass;
  Try
    AFlowPanel.OnResize := nil;
    AFlowPanel.DisableAlign;
    LClasses := GetButtonFamilyClasses(AFamily);
    LDefaultClass := LClasses[0];

    //Build Buttons for Family/Class/Appearance
    for J := 0 to Length(LClasses)-1 do
      CreateButton(AFlowPanel, LClasses[J]);
  Finally
    AFlowPanel.OnResize := FlowPanelResize;
    AFlowPanel.EnableAlign;
    Screen.Cursor := crDefault;
  End;
end;

end.
