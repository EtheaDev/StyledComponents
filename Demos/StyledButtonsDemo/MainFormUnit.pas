{******************************************************************************}
{                                                                              }
{       StyledButton: a Button Component based on TGraphicControl              }
{                                                                              }
{       Copyright (c) 2022 (Ethea S.r.l.)                                      }
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
unit MainFormUnit;

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
  Vcl.BootstrapButtonStyles,
  Vcl.AngularButtonStyles,
  Vcl.StandardButtonStyles,
  System.Actions,
  Vcl.ActnList,
  Vcl.ButtonStylesAttributes,
  Vcl.StyledButtonEditorUnit,
  Vcl.ImageCollection,
  Vcl.Menus,
  Vcl.ComCtrls,
  DResources;

type
  TMainForm = class(TForm)
    ActionList: TActionList;
    TestAction: TAction;
    PopupMenu: TPopupMenu;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Exit1: TMenuItem;
    Panel1: TPanel;
    StyledButton: TStyledButton;
    StyledButton2: TStyledButton;
    Button1: TButton;
    StyleLabel: TLabel;
    cbChangeStyle: TComboBox;
    PageControl1: TPageControl;
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
    GroupBox1: TGroupBox;
    btn_IconHome: TStyledButton;
    btn_IconDots: TStyledButton;
    btn_IconMenu: TStyledButton;
    btn_IconHeart: TStyledButton;
    btn_IconLaunchDisabled: TStyledButton;
    GroupBox2: TGroupBox;
    btn_FABTrash: TStyledButton;
    btn_FABBookmark: TStyledButton;
    VirtualImageList32: TVirtualImageList;
    btn_FABHome: TStyledButton;
    btn_FABHeartDisabled: TStyledButton;
    StyledButton1: TStyledButton;
    procedure TestActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbChangeStyleSelect(Sender: TObject);
    procedure LinkLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure rgAngularLightThemesClick(Sender: TObject);
    procedure AngularThemesPanelResize(Sender: TObject);
    procedure rgAngularDarkThemesClick(Sender: TObject);
  private
    procedure BuildStyleList;
  protected
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  System.TypInfo
  , Vcl.Themes
  , WinApi.ShellAPI
  ;

{ TMainForm }

procedure TMainForm.cbChangeStyleSelect(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    TStyleManager.TrySetStyle(cbChangeStyle.Text);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  BuildStyleList;
end;

procedure TMainForm.TestActionExecute(Sender: TObject);
begin
  EditStyledButton(StyledButton);
end;

procedure TMainForm.LinkLabelLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  ShellExecute(handle, 'open',
    PChar(Link), nil, nil, SW_SHOWNORMAL);
end;

procedure TMainForm.rgAngularDarkThemesClick(Sender: TObject);
begin
  TStyleManager.TrySetStyle('Windows10 SlateGray');
  rgAngularLightThemes.ItemIndex := -1;
  BuildStyleList;
  case rgAngularDarkThemes.ItemIndex of
    0: //Pink & Blue-grey
    begin
      btn_BasicPrimary.SetButtonStyle(AngularDarkTheme, btn_PrimaryPink, BasicAttr);
      btn_RaisedPrimary.SetButtonStyle(AngularDarkTheme, btn_PrimaryPink, RaisedAttr);
      btn_StrokedPrimary.SetButtonStyle(AngularDarkTheme, btn_PrimaryPink, StrokedAttr);
      btn_FlatPrimary.SetButtonStyle(AngularDarkTheme, btn_PrimaryPink, FlatAttr);
      btn_BasicAccent.SetButtonStyle(AngularDarkTheme, btn_AccentBlueGray, BasicAttr);
      btn_RaisedAccent.SetButtonStyle(AngularDarkTheme, btn_AccentBlueGray, RaisedAttr);
      btn_StrokedAccent.SetButtonStyle(AngularDarkTheme, btn_AccentBlueGray, StrokedAttr);
      btn_FlatAccent.SetButtonStyle(AngularDarkTheme, btn_AccentBlueGray, FlatAttr);
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
      btn_BasicPrimary.SetButtonStyle(AngularDarkTheme, btn_PrimaryPurple, BasicAttr);
      btn_RaisedPrimary.SetButtonStyle(AngularDarkTheme, btn_PrimaryPurple, RaisedAttr);
      btn_StrokedPrimary.SetButtonStyle(AngularDarkTheme, btn_PrimaryPurple, StrokedAttr);
      btn_FlatPrimary.SetButtonStyle(AngularDarkTheme, btn_PrimaryPurple, FlatAttr);
      btn_BasicAccent.SetButtonStyle(AngularDarkTheme, btn_AccentGreen, BasicAttr);
      btn_RaisedAccent.SetButtonStyle(AngularDarkTheme, btn_AccentGreen, RaisedAttr);
      btn_StrokedAccent.SetButtonStyle(AngularDarkTheme, btn_AccentGreen, StrokedAttr);
      btn_FlatAccent.SetButtonStyle(AngularDarkTheme, btn_AccentGreen, FlatAttr);
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
end;

procedure TMainForm.rgAngularLightThemesClick(Sender: TObject);
begin
  TStyleManager.TrySetStyle('Windows10');
  rgAngularDarkThemes.ItemIndex := -1;
  BuildStyleList;
  case rgAngularLightThemes.ItemIndex of
    0: //Deep Purple & Amber
    begin
      btn_BasicPrimary.SetButtonStyle(AngularLightTheme, btn_PrimaryDeepPurple, BasicAttr);
      btn_RaisedPrimary.SetButtonStyle(AngularLightTheme, btn_PrimaryDeepPurple, RaisedAttr);
      btn_StrokedPrimary.SetButtonStyle(AngularLightTheme, btn_PrimaryDeepPurple, StrokedAttr);
      btn_FlatPrimary.SetButtonStyle(AngularLightTheme, btn_PrimaryDeepPurple, FlatAttr);
      btn_BasicAccent.SetButtonStyle(AngularLightTheme, btn_AccentAmber, BasicAttr);
      btn_RaisedAccent.SetButtonStyle(AngularLightTheme, btn_AccentAmber, RaisedAttr);
      btn_StrokedAccent.SetButtonStyle(AngularLightTheme, btn_AccentAmber, StrokedAttr);
      btn_FlatAccent.SetButtonStyle(AngularLightTheme, btn_AccentAmber, FlatAttr);
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
      btn_BasicPrimary.SetButtonStyle(AngularLightTheme, btn_PrimaryIndigo, BasicAttr);
      btn_RaisedPrimary.SetButtonStyle(AngularLightTheme, btn_PrimaryIndigo, RaisedAttr);
      btn_StrokedPrimary.SetButtonStyle(AngularLightTheme, btn_PrimaryIndigo, StrokedAttr);
      btn_FlatPrimary.SetButtonStyle(AngularLightTheme, btn_PrimaryIndigo, FlatAttr);
      btn_BasicAccent.SetButtonStyle(AngularLightTheme, btn_AccentPink, BasicAttr);
      btn_RaisedAccent.SetButtonStyle(AngularLightTheme, btn_AccentPink, RaisedAttr);
      btn_StrokedAccent.SetButtonStyle(AngularLightTheme, btn_AccentPink, StrokedAttr);
      btn_FlatAccent.SetButtonStyle(AngularLightTheme, btn_AccentPink, FlatAttr);
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
end;

procedure TMainForm.AngularThemesPanelResize(Sender: TObject);
begin
  rgAngularLightThemes.Width := AngularThemesPanel.Width div 2;
end;

procedure TMainForm.BuildStyleList;
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

initialization
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
end.
