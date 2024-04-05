{******************************************************************************}
{                                                                              }
{  TStyledDbNavigator and TStyledBindNavigator Test                            }
{  Compared to TDbNavigator and TBindNavigator                                 }
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
unit MainFormOld;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ToolWin, Vcl.ActnMan,
  Vcl.ActnCtrls, Vcl.ImgList, Vcl.StdCtrls, Vcl.Menus,
  Vcl.DBCtrls, Data.DB, Vcl.Grids, Vcl.DBGrids, Datasnap.DBClient, Vcl.Buttons,
  Data.Bind.Controls,
  Data.Bind.Components, Data.Bind.DBScope, Vcl.Bind.Navigator,
  Vcl.StyledButton, Vcl.ButtonStylesAttributes,
  Vcl.StandardButtonStyles, Vcl.AngularButtonStyles,
  Vcl.BootstrapButtonStyles, Vcl.ColorButtonStyles, Vcl.StyledDbNavigator,
  System.ImageList, Vcl.VirtualImageList;

const
  //Params to check
  FLAT_BUTTONS = True;
  DBNAV_HEIGHT = 50;
  DBNAV_WIDTH = 50;
  SHOW_CAPTIONS = False;

  //Test alignment
  //DBNAV_ALIGN = alTop;
  //DBNAV_KIND = dbnHorizontal;
  DBNAV_ALIGN = alLeft;
  DBNAV_KIND = dbnVertical;

type
  TfmMain = class(TForm)
    BottomPanel: TPanel;
    ClientPanel: TPanel;
    LeftPanel: TPanel;
    CreateButton: TStyledButton;
    BottomClientPanel: TPanel;
    WidthLabel: TLabel;
    HeightLabel: TLabel;
    tbWidth: TTrackBar;
    tbHeight: TTrackBar;
    ShowCaptionsCheckBox: TCheckBox;
    FlatCheckBox: TCheckBox;
    PopupMenu: TPopupMenu;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Exit1: TMenuItem;
    Panel1: TPanel;
    cbChangeStyle: TComboBox;
    StyleLabel: TLabel;
    DBNavigator: TDBNavigator;
    ClientDataSet: TClientDataSet;
    ClientDataSetID: TAutoIncField;
    ClientDataSetCustomerID: TIntegerField;
    ClientDataSetProductID: TIntegerField;
    ClientDataSetPurchaseDate: TDateTimeField;
    ClientDataSetTime: TDateTimeField;
    ClientDataSetPaymentType: TStringField;
    ClientDataSetPaymentAmount: TCurrencyField;
    ClientDataSetDescription: TMemoField;
    ClientDataSetQuantity: TIntegerField;
    DBGrid1: TDBGrid;
    DataSource: TDataSource;
    StyledDBNavigator: TStyledDbNavigator;
    StyledBindNavigator: TStyledBindNavigator;
    CustomImagesCheckBox: TCheckBox;
    BindNavigator: TBindNavigator;
    BindSourceDB: TBindSourceDB;
    ImageList32: TImageList;
    VCLButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ToolButtonclick(Sender: TObject);
    procedure CreateButtonClick(Sender: TObject);
    procedure UpdateDbNav(Sender: TObject);
    procedure PopUpMenuClick(Sender: TObject);
    procedure cbChangeStyleSelect(Sender: TObject);
    procedure CustomImagesCheckBoxClick(Sender: TObject);
  private
    FDbNavigator: TDbNavigator;
    FStyledDbNavigator: TStyledDbNavigator;
    FStyledBindNavigator: TStyledBindNavigator;
    procedure CreateStyledDbNavigator;
    procedure CreateStyledBindNavigator;
    procedure CreateDbNavigators;
    procedure CreateDbNavigator;
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

procedure TfmMain.CreateButtonClick(Sender: TObject);
begin
  CreateDbNavigators;
  LeftPanel.Visible := False;
  BottomClientPanel.Visible := True;
end;

procedure TfmMain.CreateStyledDbNavigator;
begin
  FStyledDbNavigator := TStyledDbNavigator.Create(Self);
  FStyledDbNavigator.Flat := FLAT_BUTTONS;
  FStyledDbNavigator.ShowCaptions := SHOW_CAPTIONS;
  FStyledDbNavigator.Align := DBNAV_ALIGN;
  FStyledDbNavigator.Kind := DBNAV_KIND;
  FStyledDbNavigator.Height := DBNAV_HEIGHT;
  FStyledDbNavigator.Width := DBNAV_WIDTH;
  FStyledDbNavigator.VisibleButtons := [TNavigateBtn.nbFirst, TNavigateBtn.nbPrior, TNavigateBtn.nbNext, TNavigateBtn.nbLast];
  FStyledDbNavigator.DataSource := DataSource;
  FStyledDbNavigator.Parent := Self;
end;

procedure TfmMain.CreateStyledBindNavigator;
begin
  FStyledBindNavigator := TStyledBindNavigator.Create(Self);
  FStyledBindNavigator.Flat := FLAT_BUTTONS;
  FStyledBindNavigator.ShowCaptions := SHOW_CAPTIONS;
  FStyledBindNavigator.Align := DBNAV_ALIGN;
  FStyledBindNavigator.Kind := DBNAV_KIND;
  FStyledBindNavigator.Height := DBNAV_HEIGHT;
  FStyledBindNavigator.Width := DBNAV_WIDTH;
  FStyledBindNavigator.VisibleButtons :=
    [TNavigateButton.nbFirst, TNavigateButton.nbPrior, TNavigateButton.nbNext, TNavigateButton.nbLast];
  FStyledBindNavigator.DataSource := BindSourceDB;
  FStyledBindNavigator.Parent := Self;
end;

procedure TfmMain.CustomImagesCheckBoxClick(Sender: TObject);
begin
  if StyledDBNavigator.Images = ImageList32 then
  begin
    FStyledDBNavigator.Images := nil;
    FStyledBindNavigator.Images := nil;
    StyledDBNavigator.Images := nil;
    StyledBindNavigator.Images := nil;
  end
  else
  begin
    FStyledDBNavigator.Images := ImageList32;
    FStyledBindNavigator.Images := ImageList32;
    StyledDBNavigator.Images := ImageList32;
    StyledBindNavigator.Images := ImageList32;
  end;
end;

procedure TfmMain.CreateDbNavigator;
begin
  FDbNavigator := TDbNavigator.Create(Self);
  FDbNavigator.Flat := FLAT_BUTTONS;
  FDbNavigator.Align := DBNAV_ALIGN;
  FDbNavigator.Kind := DBNAV_KIND;
  FDbNavigator.Height := DBNAV_HEIGHT;
  FDbNavigator.Width := DBNAV_WIDTH;
  FDbNavigator.StyleElements := [];
  FDbNavigator.VisibleButtons := [TNavigateBtn.nbFirst, TNavigateBtn.nbPrior, TNavigateBtn.nbNext, TNavigateBtn.nbLast];
  FDbNavigator.DataSource := DataSource;
  FDbNavigator.Parent := Self;
end;

procedure TfmMain.CreateDbNavigators;
begin
  //Create Standard DbNavigator
  CreateDbNavigator;

  //Create Styled DbNavigator
  CreateStyledDbNavigator;

  //Create Styled Bind Navigator
  CreateStyledBindNavigator;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  BuildStyleList;
  Caption := Application.Title;
  FlatCheckBox.Checked := FLAT_BUTTONS;
  ShowCaptionsCheckBox.Checked := SHOW_CAPTIONS;
  tbWidth.Position := DBNAV_WIDTH;
  tbHeight.Position := DBNAV_HEIGHT;
end;

procedure TfmMain.PopUpMenuClick(Sender: TObject);
begin
  ShowMessage((Sender as TMenuItem).Caption);
end;

procedure TfmMain.ToolButtonclick(Sender: TObject);
begin
  if Sender is TToolButton then
    ShowMessage(TToolButton(Sender).Caption)
  else if Sender is TStyledNavButton then
    EditStyledButton(TStyledNavButton(Sender));
end;

procedure TfmMain.UpdateDbNav(Sender: TObject);
begin
  if Assigned(FStyledDbNavigator) then
  begin
    FStyledDbNavigator.Flat := FlatCheckBox.Checked;
    FStyledDbNavigator.ShowCaptions := ShowCaptionsCheckBox.Checked;
    FStyledDbNavigator.Height := tbHeight.Position;
    FStyledDbNavigator.Width := tbWidth.Position;
  end;
  if Assigned(FStyledBindNavigator) then
  begin
    FStyledBindNavigator.Flat := FlatCheckBox.Checked;
    FStyledBindNavigator.ShowCaptions := ShowCaptionsCheckBox.Checked;
    FStyledBindNavigator.Height := tbHeight.Position;
    FStyledBindNavigator.Width := tbWidth.Position;
  end;
  if Assigned(FDbNavigator) then
  begin
    FDbNavigator.Flat := FlatCheckBox.Checked;
    FDbNavigator.Width := tbWidth.Position;
    FDbNavigator.Height := tbHeight.Position;
  end;
  DbNavigator.Flat := FlatCheckBox.Checked;
  DbNavigator.Width := tbWidth.Position;
  DbNavigator.Height := tbHeight.Position;
  BindNavigator.Flat := FlatCheckBox.Checked;
  BindNavigator.Width := tbWidth.Position;
  BindNavigator.Height := tbHeight.Position;
  StyledDbNavigator.Flat := FlatCheckBox.Checked;
  StyledDbNavigator.ShowCaptions := ShowCaptionsCheckBox.Checked;
  StyledDbNavigator.Height := tbHeight.Position;
  StyledDbNavigator.Width := tbWidth.Position;
  StyledBindNavigator.Flat := FlatCheckBox.Checked;
  StyledBindNavigator.ShowCaptions := ShowCaptionsCheckBox.Checked;
  StyledBindNavigator.Height := tbHeight.Position;
  StyledBindNavigator.Width := tbWidth.Position;
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
