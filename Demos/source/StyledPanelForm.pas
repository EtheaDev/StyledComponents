{******************************************************************************}
{                                                                              }
{  StyledPanelDemo: a Demo to show StyledPanel                                 }
{  with different Families (Classic, Bootstrap, Angular, Basic-Color, etc.)    }
{                                                                              }
{  Copyright (c) 2022-2025 (Ethea S.r.l.)                                      }
{  Author: Carlo Barazzetta                                                    }
{  Contributors: Claude Code                                                   }
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
unit StyledPanelForm;

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
  Vcl.ComCtrls,
  Vcl.StyledPanel,
  Vcl.ButtonStylesAttributes,
  Vcl.ImgList,
  System.ImageList, 
  Vcl.VirtualImageList;

const
  //Dimensions ad VCL TPanel
  PANEL_WIDTH = 185;
  PANEL_HEIGHT = 41;

type
  TfmStyledPanel = class(TForm)
    PageControl: TPageControl;
    tsClassic: TTabSheet;
    tsBootstrap: TTabSheet;
    tsAngular: TTabSheet;
    tsBasicColor: TTabSheet;
    tsSVGColor: TTabSheet;
    ClassicScrollBox: TScrollBox;
    ClassicNormalGroupBox: TGroupBox;
    ClassicNormalFlowPanel: TFlowPanel;
    ClassicOutlineGroupBox: TGroupBox;
    ClassicOutlineFlowPanel: TFlowPanel;
    BootstrapScrollBox: TScrollBox;
    BootstrapNormalGroupBox: TGroupBox;
    BootstrapNormalFlowPanel: TFlowPanel;
    BootstrapOutlineGroupBox: TGroupBox;
    BootstrapOutlineFlowPanel: TFlowPanel;
    AngularScrollBox: TScrollBox;
    AngularRaisedGroupBox: TGroupBox;
    AngularRaisedFlowPanel: TFlowPanel;
    AngularStrokedGroupBox: TGroupBox;
    AngularStrokedFlowPanel: TFlowPanel;
    BasicColorScrollBox: TScrollBox;
    BasicColorNormalGroupBox: TGroupBox;
    BasicColorNormalFlowPanel: TFlowPanel;
    BasicColorOutlineGroupBox: TGroupBox;
    BasicColorOutlineFlowPanel: TFlowPanel;
    SvgColorScrollBox: TScrollBox;
    SvgColorNormalGroupBox: TGroupBox;
    SvgColorNormalFlowPanel: TFlowPanel;
    SvgColorOutlineGroupBox: TGroupBox;
    SvgColorOutlineFlowPanel: TFlowPanel;
    VirtualImageList32: TVirtualImageList;
    AngularBasicGroupBox: TGroupBox;
    AngularBasicFlowPanel: TFlowPanel;
    AngularFlatGroupBox: TGroupBox;
    AngularFlatFlowPanel: TFlowPanel;
    procedure FormCreate(Sender: TObject);
    procedure FlowPanelResize(Sender: TObject);
    procedure ScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    procedure BuildFamilyPreview(const AFamily: TStyledButtonFamily;
      const AAppearance: TStyledButtonAppearance;
      const AFlowPanel: TFlowPanel);
  end;

var
  fmStyledPanel: TfmStyledPanel;

implementation

{$R *.dfm}

uses
  Vcl.StandardButtonStyles,
  Vcl.BootstrapButtonStyles,
  Vcl.AngularButtonStyles,
  Vcl.ColorButtonStyles;

procedure TfmStyledPanel.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  PageControl.ActivePageIndex := 0;

  // Build Classic family panels
  BuildFamilyPreview(DEFAULT_CLASSIC_FAMILY, DEFAULT_APPEARANCE, ClassicNormalFlowPanel);
  BuildFamilyPreview(DEFAULT_CLASSIC_FAMILY, OUTLINE_APPEARANCE, ClassicOutlineFlowPanel);

  // Build Bootstrap family panels
  BuildFamilyPreview(BOOTSTRAP_FAMILY, BOOTSTRAP_NORMAL, BootstrapNormalFlowPanel);
  BuildFamilyPreview(BOOTSTRAP_FAMILY, BOOTSTRAP_OUTLINE, BootstrapOutlineFlowPanel);

  // Build Angular family panels (using Light theme)
  BuildFamilyPreview(ANGULAR_LIGHT_FAMILY, RaisedAttr, AngularRaisedFlowPanel);
  BuildFamilyPreview(ANGULAR_LIGHT_FAMILY, StrokedAttr, AngularStrokedFlowPanel);
  BuildFamilyPreview(ANGULAR_LIGHT_FAMILY, BasicAttr, AngularBasicFlowPanel);
  BuildFamilyPreview(ANGULAR_LIGHT_FAMILY, FlatAttr, AngularFlatFlowPanel);

  // Build Basic-Color family panels
  BuildFamilyPreview(BASIC_COLOR_FAMILY, COLOR_BTN_NORMAL, BasicColorNormalFlowPanel);
  BuildFamilyPreview(BASIC_COLOR_FAMILY, COLOR_BTN_OUTLINE, BasicColorOutlineFlowPanel);

  // Build SVG-Color family panels
  BuildFamilyPreview(SVG_COLOR_FAMILY, COLOR_BTN_NORMAL, SvgColorNormalFlowPanel);
  BuildFamilyPreview(SVG_COLOR_FAMILY, COLOR_BTN_OUTLINE, SvgColorOutlineFlowPanel);

  PageControl.ActivePage := tsClassic;
end;

procedure TfmStyledPanel.BuildFamilyPreview(
  const AFamily: TStyledButtonFamily;
  const AAppearance: TStyledButtonAppearance;
  const AFlowPanel: TFlowPanel);
var
  J: Integer;
  LClasses: TButtonClasses;

  procedure CreatePanel(
    const AParent: TFlowPanel;
    const AClass: TStyledButtonClass);
  var
    LStyledPanel: TStyledPanel;
  begin
    LStyledPanel := TStyledPanel.CreateStyled(Self,
      AFamily, AClass, AAppearance);
    LStyledPanel.Width := PANEL_WIDTH;
    LStyledPanel.Height := PANEL_HEIGHT;
    LStyledPanel.AlignWithMargins := True;
    LStyledPanel.Caption := AClass;
    LStyledPanel.Hint := Format('%s/%s/%s', [AFamily, AClass, AAppearance]);
    LStyledPanel.ShowHint := True;
    LStyledPanel.Parent := AParent;
  end;

begin
  if AFlowPanel.ControlCount > 0 then
    Exit;

  Screen.Cursor := crHourGlass;
  Try
    AFlowPanel.OnResize := nil;
    AFlowPanel.DisableAlign;
    LClasses := GetButtonFamilyClasses(AFamily);

    //Build Panels for Family/Class/Appearance
    for J := 0 to Length(LClasses) - 1 do
      CreatePanel(AFlowPanel, LClasses[J]);
  Finally
    AFlowPanel.OnResize := FlowPanelResize;
    AFlowPanel.EnableAlign;
    Screen.Cursor := crDefault;
  End;
end;

procedure TfmStyledPanel.FlowPanelResize(Sender: TObject);
begin
  TFlowPanel(Sender).Parent.Height := TFlowPanel(Sender).Height + Round(20 * ScaleFactor);
end;

procedure TfmStyledPanel.ScrollBoxMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  LScrollBox: TScrollBox;
begin
  if Sender is TScrollBox then
  begin
    LScrollBox := TScrollBox(Sender);
    LScrollBox.VertScrollBar.Position :=
      LScrollBox.VertScrollBar.Position - (WheelDelta div 4);
    Handled := True;
  end;
end;

end.
