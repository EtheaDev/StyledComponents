{******************************************************************************}
{                                                                              }
{  StyledComponents: a set of Styled VCL Component                             }
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
unit Vcl.StyledComponentsRegister;

interface

{$INCLUDE ..\Source\StyledComponents.inc}

uses
  Classes
  , DesignIntf
  , Designer
  , DesignEditors
  , VCLEditors
  , Vcl.ImgList
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.ComCtrls
  , System.Types
  , Vcl.StyledButton
  , Vcl.StyledToolbar
  , Vcl.StyledDbNavigator
  , Vcl.StyledButtonGroup
  , Vcl.StyledCategoryButtons
  , Vcl.ButtonStylesAttributes
  , Vcl.StyledTaskDialog
  ;

Type
  TStyledFamilyPropertyEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TStyledClassPropertyEditor = class(TStringProperty)
  protected
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TStyledAppearancePropertyEditor = class(TStringProperty)
  protected
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TStyledButtonComponentEditor = class(TComponentEditor)
  private
    function GetButton: TControl;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

  TStyledToolbarComponentEditor = class(TComponentEditor)
  private
    function GetToolbar: TStyledToolbar;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TStyledNavigatorComponentEditor = class(TComponentEditor)
  private
    function GetDbNavigator: TCustomStyledDbNavigator;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TStyledButtonGroupComponentEditor = class(TComponentEditor)
  private
    function GetButtonGroup: TStyledButtonGroup;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TStyledCategoryButtonsComponentEditor = class(TComponentEditor)
  private
    function GetCategoryButtons: TStyledCategoryButtons;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TStyledTaskDialogComponentEditor = class(TComponentEditor)
  private
    function GetTaskDialog: TStyledTaskDialog;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TStyledComponentSelection = class (TSelectionEditor, ISelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TImageIndexPropertyEditor = class(TIntegerProperty, ICustomPropertyListDrawing)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetImageListAt(Index: Integer): TCustomImageList; virtual;
    // ICustomPropertyListDrawing
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);
  end;

  {$IFDEF D10_4}
  TImageNamePropertyEditor = class(TStringProperty, ICustomPropertyListDrawing)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetImageListAt(Index: Integer): TCustomImageList; virtual;
    // ICustomPropertyListDrawing
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);
  end;
  {$ENDIF}

procedure Register;

implementation

uses
  Vcl.StandardButtonStyles
  , Vcl.BootstrapButtonStyles
  , Vcl.AngularButtonStyles
  , Vcl.StyledButtonEditorUnit
  , Vcl.StyledCmpStrUtils
  , Vcl.DbCtrls
  , Vcl.ButtonGroup
  , Vcl.CategoryButtons
  , System.SysUtils
  , System.Contnrs
  , System.UITypes
  , Winapi.ShellAPI
  , Winapi.Windows
  ;


function GetComponentFamilyClass(const AComponent: TPersistent;
  out AButtonFamily: TButtonFamily): boolean;
var
  LFamily: TStyledButtonFamily;
begin
  LFamily := '';
  if AComponent is TCustomStyledGraphicButton then
    LFamily := TCustomStyledGraphicButton(AComponent).StyleFamily
  else if AComponent is TCustomStyledButton then
    LFamily := TCustomStyledButton(AComponent).StyleFamily
  else if AComponent is TStyledToolbar then
    LFamily := TStyledToolbar(AComponent).StyleFamily
  else if AComponent is TStyledToolButton then
    LFamily := TStyledToolButton(AComponent).StyleFamily
  else if AComponent is TCustomStyledDbNavigator then
    LFamily := TCustomStyledDbNavigator(AComponent).StyleFamily
  else if AComponent is TStyledButtonGroup then
    LFamily := TStyledButtonGroup(AComponent).StyleFamily
  else if AComponent is TStyledGrpButtonItem then
    LFamily := TStyledGrpButtonItem(AComponent).StyleFamily
  else if AComponent is TStyledCategoryButtons then
    LFamily := TStyledCategoryButtons(AComponent).StyleFamily
  else if AComponent is TStyledButtonItem then
    LFamily := TStyledButtonItem(AComponent).StyleFamily;
  if LFamily <> '' then
  begin
    Result := True;
    AButtonFamily := GetButtonFamilyClass(LFamily);
  end
  else
  begin
    AButtonFamily := nil;
    Result := False;
  end;

end;

{ TStyledFamilyPropertyEditor }

function TStyledFamilyPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect];
end;

procedure TStyledFamilyPropertyEditor.GetValues(Proc: TGetStrProc);
var
  LFamilies: TObjectList;
  I: Integer;
begin
  LFamilies := GetButtonFamilies;
  if Assigned(LFamilies) then
  begin
    for I := 0 to LFamilies.Count -1 do
      Proc(GetButtonFamilyName(I));
  end;
end;


{ TStyledClassPropertyEditor }

function TStyledClassPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect];
end;

procedure TStyledClassPropertyEditor.GetValues(Proc: TGetStrProc);
var
  LClasses: TButtonClasses;
  I: Integer;
  LFamilyClass: TButtonFamily;
begin
  if GetComponentFamilyClass(GetComponent(0), LFamilyClass) then
  begin
    LClasses := GetButtonClasses(LFamilyClass);
    if Assigned(LClasses) then
    begin
      for I := Low(LClasses) to High(LClasses) do
        Proc(LClasses[I]);
    end;
  end;
end;

{ TStyledAppearancePropertyEditor }

function TStyledAppearancePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect];
end;

procedure TStyledAppearancePropertyEditor.GetValues(Proc: TGetStrProc);
var
  LAppearances: TButtonAppearances;
  I: Integer;
  LFamilyClass: TButtonFamily;
begin
  if GetComponentFamilyClass(GetComponent(0), LFamilyClass) then
  begin
    LAppearances := GetButtonAppearances(LFamilyClass);
    if Assigned(LAppearances) then
    begin
      for I := Low(LAppearances) to High(LAppearances) do
        Proc(LAppearances[I]);
    end;
  end;
end;

{ TStyledButtonComponentEditor }

function TStyledButtonComponentEditor.GetButton: TControl;
var
  LComponent: TPersistent;
begin
  Result := nil;
  LComponent := GetComponent;
  if LComponent is TCustomStyledGraphicButton then
    Result := TCustomStyledGraphicButton(LComponent)
  else if LComponent is TCustomStyledButton then
    Result := TCustomStyledButton(LComponent);
end;

procedure TStyledButtonComponentEditor.Edit;
begin
  inherited;
end;

procedure TStyledButtonComponentEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  if Index = 0 then
  begin
    if EditStyledButton(GetButton) then
      Designer.Modified;
  end
  else if Index = 1 then
  ShellExecute(0, 'open',
    PChar(GetProjectURL), nil, nil, SW_SHOWNORMAL);
end;

function TStyledButtonComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Styled Button Editor...'
  else if Index = 1 then
    Result := 'Project page on GitHub...';
end;

function TStyledButtonComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TStyledComponentSelection }
procedure TStyledComponentSelection.RequiresUnits(Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);
  Proc('Vcl.ButtonStylesAttributes');
end;

{ TStyledToolbarComponentEditor }

procedure TStyledToolbarComponentEditor.ExecuteVerb(Index: Integer);
var
  LToolbar: TStyledToolbar;
  LNewButton: TStyledToolButton;
begin
  inherited;
  if Index = 0 then //Show Styled Toolbar Editor...
  begin
    LToolbar := GetToolbar;
    LNewButton := TStyledToolButton.CreateStyled(LToolbar.Owner,
      LToolbar.StyleFamily, LToolbar.StyleClass, LToolbar.StyleAppearance);
    try
      LNewButton.StyleDrawType := LToolbar.StyleDrawType;
      LNewButton.StyleRadius := LToolbar.StyleRadius;
      LNewButton.SetBounds(0, 0, LToolbar.ButtonWidth, LToolbar.ButtonHeight);
      if LToolbar.ShowCaptions then
      begin
        LNewButton.Name := Designer.UniqueName('Button');
        LNewButton.Caption := 'Button';
      end;
      if EditStyledButton(LNewButton) then
      begin
        LToolbar.SetToolbarStyle(LNewButton.StyleFamily,
          LNewButton.StyleClass, LNewButton.StyleAppearance);
        LToolbar.StyleRadius := LNewButton.StyleRadius;
        LToolbar.StyleDrawType := LNewButton.StyleDrawType;
        Designer.Modified;
      end;
    finally
      LNewButton.Free;
    end;
  end
  else if Index = 1 then //Add a StyledToolButton
  begin
    LToolbar := GetToolbar;
    if LToolbar.NewButton(LNewButton, tbsButton) then
    begin
      //Set unique Name to the Button created
      LNewButton.Name := Designer.UniqueName('StyledToolButton');
      LNewButton.Caption := LNewButton.Name;

      //Select the button into Designer
      Designer.SelectComponent(LNewButton);
      Designer.Modified;
    end;
  end
  else if Index = 2 then //Add a StyledToolButton as Separator
  begin
    if GetToolbar.NewButton(LNewButton, tbsSeparator) then
    begin
      //Set unique Name to the Button created
      LNewButton.Name := Designer.UniqueName('StyledToolButton');
      LNewButton.Caption := LNewButton.Name;
      //Select the button into Designer
      Designer.SelectComponent(LNewButton);
      Designer.Modified;
    end;
  end
  else if Index = 3 then //Open Project page on GitHub...
  ShellExecute(0, 'open',
    PChar(GetProjectURL), nil, nil, SW_SHOWNORMAL);
end;

function TStyledToolbarComponentEditor.GetToolbar: TStyledToolbar;
var
  LComponent: TPersistent;
begin
  Result := nil;
  LComponent := GetComponent;
  if LComponent is TStyledToolbar then
    Result := TStyledToolbar(LComponent);
end;

function TStyledToolbarComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Styled Toolbar Editor...'
  else if Index = 1 then
    Result := 'Add StyledToolbar Button'
  else if Index = 2 then
    Result := 'Add StyledToolbar Separator'
  else if Index = 3 then
    Result := 'Project page on GitHub...';
end;

function TStyledToolbarComponentEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

{ TStyledNavigatorComponentEditor }

procedure TStyledNavigatorComponentEditor.ExecuteVerb(Index: Integer);
var
  LDbNavigator: TCustomStyledDbNavigator;
  LNavButton: TStyledNavButton;
begin
  inherited;
  if Index = 0 then
  begin
    LDbNavigator := GetDbNavigator;
    LNavButton := TStyledNavButton.CreateStyled(LDbNavigator,
      LDbNavigator.StyleFamily, LDbNavigator.StyleClass, LDbNavigator.StyleAppearance,
      LDbNavigator.StyleDrawType, LDbNavigator.Cursor, False);
    try
      LNavButton.StyleDrawType := LDbNavigator.StyleDrawType;
      LNavButton.StyleRadius := LDbNavigator.StyleRadius;
      LNavButton.SetBounds(0, 0, LDbNavigator.ButtonWidth, LDbNavigator.ButtonHeight);
      if LDbNavigator.ShowCaptions then
      begin
        LNavButton.Name := Designer.UniqueName('Button');
        LNavButton.Caption := 'Button';
      end;
      if EditStyledButton(LNavButton) then
      begin
        LDbNavigator.SetDbNavigatorStyle(LNavButton.StyleFamily,
          LNavButton.StyleClass, LNavButton.StyleAppearance);
        LDbNavigator.StyleRadius := LNavButton.StyleRadius;
        LDbNavigator.StyleDrawType := LNavButton.StyleDrawType;
        Designer.Modified;
      end;
    finally
      LNavButton.Free;
    end;
  end
  else if Index = 1 then
  ShellExecute(0, 'open',
    PChar(GetProjectURL), nil, nil, SW_SHOWNORMAL);
end;

function TStyledNavigatorComponentEditor.GetDbNavigator: TCustomStyledDbNavigator;
var
  LComponent: TPersistent;
begin
  Result := nil;
  LComponent := GetComponent;
  if LComponent is TCustomStyledDbNavigator then
    Result := TCustomStyledDbNavigator(LComponent);
end;

function TStyledNavigatorComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Styled DbNavigator Editor...'
  else if Index = 1 then
    Result := 'Project page on GitHub...';
end;

function TStyledNavigatorComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TStyledButtonGroupComponentEditor }

procedure TStyledButtonGroupComponentEditor.ExecuteVerb(Index: Integer);
var
  LButtonGroup: TStyledbuttonGroup;
  LButton: TStyledButton;
  LButtonWidth: Integer;
begin
  if Index = 0 then
  begin
    LButtonGroup := GetButtonGroup;
    LButton := TStyledButton.CreateStyled(LButtonGroup,
      LButtonGroup.StyleFamily, LButtonGroup.StyleClass, LButtonGroup.StyleAppearance,
      LButtonGroup.StyleDrawType, LButtonGroup.ButtonsCursor, False);
    try
      LButton.StyleRadius := LButtonGroup.StyleRadius;
      LButton.StyleRoundedCorners := LButtonGroup.StyleRoundedCorners;
      LButton.StyleDrawType := LButtonGroup.StyleDrawType;
      if gboFullSize in LButtonGroup.ButtonOptions then
        LButtonWidth := LButtonGroup.Width
      else
        LButtonWidth := LButtonGroup.ButtonWidth;
      LButton.SetBounds(0, 0, LButtonWidth, LButtonGroup.ButtonHeight);
      if gboShowCaptions in LButtonGroup.ButtonOptions then
      begin
        LButton.Name := Designer.UniqueName('Button');
        LButton.Caption := 'Button';
      end;
      if EditStyledButton(LButton) then
      begin
        LButtonGroup.SetButtonGroupStyle(LButton.StyleFamily,
          LButton.StyleClass, LButton.StyleAppearance);
        LButtonGroup.StyleRadius := LButton.StyleRadius;
        LButtonGroup.StyleRoundedCorners := LButton.StyleRoundedCorners;
        LButtonGroup.StyleDrawType := LButton.StyleDrawType;
        LButtonGroup.Invalidate;
        Designer.Modified;
      end;
    finally
      LButton.Free;
    end;
  end
  else if Index = 1 then
  ShellExecute(0, 'open',
    PChar(GetProjectURL), nil, nil, SW_SHOWNORMAL);
end;

function TStyledButtonGroupComponentEditor.GetButtonGroup: TStyledButtonGroup;
var
  LComponent: TPersistent;
begin
  Result := nil;
  LComponent := GetComponent;
  if LComponent is TStyledButtonGroup then
    Result := TStyledButtonGroup(LComponent);
end;

function TStyledButtonGroupComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Styled ButtonGroup Editor...'
  else if Index = 1 then
    Result := 'Project page on GitHub...';
end;

function TStyledButtonGroupComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TStyledCategoryButtonsComponentEditor }

procedure TStyledCategoryButtonsComponentEditor.ExecuteVerb(Index: Integer);
var
  LCategoryButtons: TStyledCategoryButtons;
  LButton: TStyledButton;
  LButtonWidth: Integer;
begin
  if Index = 0 then
  begin
    LCategoryButtons := GetCategoryButtons;
    LButton := TStyledButton.CreateStyled(LCategoryButtons,
      LCategoryButtons.StyleFamily, LCategoryButtons.StyleClass, LCategoryButtons.StyleAppearance,
      LCategoryButtons.StyleDrawType, LCategoryButtons.ButtonsCursor, False);
    try
      LButton.StyleRadius := LCategoryButtons.StyleRadius;
      LButton.StyleRoundedCorners := LCategoryButtons.StyleRoundedCorners;
      LButton.StyleDrawType := LCategoryButtons.StyleDrawType;
      if boFullSize in LCategoryButtons.ButtonOptions then
        LButtonWidth := LCategoryButtons.Width
      else
        LButtonWidth := LCategoryButtons.ButtonWidth;
      LButton.SetBounds(0, 0, LButtonWidth, LCategoryButtons.ButtonHeight);
      if boShowCaptions in LCategoryButtons.ButtonOptions then
      begin
        LButton.Name := Designer.UniqueName('Button');
        LButton.Caption := 'Button';
      end;
      if EditStyledButton(LButton) then
      begin
        LCategoryButtons.SetCategoryButtonsStyle(LButton.StyleFamily,
          LButton.StyleClass, LButton.StyleAppearance);
        LCategoryButtons.StyleRadius := LButton.StyleRadius;
        LCategoryButtons.StyleRoundedCorners := LButton.StyleRoundedCorners;
        LCategoryButtons.StyleDrawType := LButton.StyleDrawType;
        LCategoryButtons.Invalidate;
        Designer.Modified;
      end;
    finally
      LButton.Free;
    end;
  end
  else if Index = 1 then
  ShellExecute(0, 'open',
    PChar(GetProjectURL), nil, nil, SW_SHOWNORMAL);
end;

function TStyledCategoryButtonsComponentEditor.GetCategoryButtons: TStyledCategoryButtons;
var
  LComponent: TPersistent;
begin
  Result := nil;
  LComponent := GetComponent;
  if LComponent is TStyledCategoryButtons then
    Result := TStyledCategoryButtons(LComponent);
end;

function TStyledCategoryButtonsComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Styled CategoryButtons Editor...'
  else if Index = 1 then
    Result := 'Project page on GitHub...';
end;

function TStyledCategoryButtonsComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TStyledTaskDialogComponentEditor }

procedure TStyledTaskDialogComponentEditor.ExecuteVerb(Index: Integer);
var
  LTaskDialog: TStyledTaskDialog;
begin
  if Index = 0 then
  begin
    LTaskDialog := GetTaskDialog;
    if Assigned(LTaskDialog) then
      LTaskDialog.Execute;
  end
  else if Index = 1 then
  ShellExecute(0, 'open',
    PChar(GetProjectURL), nil, nil, SW_SHOWNORMAL);
end;

function TStyledTaskDialogComponentEditor.GetTaskDialog: TStyledTaskDialog;
var
  LComponent: TPersistent;
begin
  Result := nil;
  LComponent := GetComponent;
  if LComponent is TStyledTaskDialog then
    Result := TStyledTaskDialog(LComponent);
end;

function TStyledTaskDialogComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Test Dialog...'
  else if Index = 1 then
    Result := 'Project page on GitHub...';
end;

function TStyledTaskDialogComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TImageIndexPropertyEditor }

function TImageIndexPropertyEditor.GetImageListAt(Index: Integer): TCustomImageList;
var
  LComponent: TPersistent;
begin
  Result := nil;
  LComponent := GetComponent(Index);
  if LComponent is TCustomStyledButton then
    Result := TCustomStyledButton(LComponent).Images
  else if LComponent is TCustomStyledGraphicButton then
    Result := TCustomStyledGraphicButton(LComponent).Images;
end;

function TImageIndexPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TImageIndexPropertyEditor.GetValues(Proc: TGetStrProc);
var
  ImgList: TCustomImageList;
  I: Integer;
begin
  ImgList := GetImageListAt(0);
  if Assigned(ImgList) then
    for I := 0 to ImgList.Count -1 do
      Proc(IntToStr(I));
end;

procedure TImageIndexPropertyEditor.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  ImgList: TCustomImageList;
  X: Integer;
begin
  ImgList := GetImageListAt(0);
  ACanvas.FillRect(ARect);
  X := ARect.Left + 2;
  if Assigned(ImgList) then
  begin
    ImgList.Draw(ACanvas, X, ARect.Top + 2, StrToInt(Value));
    Inc(X, ImgList.Width);
  end;
  ACanvas.TextOut(X + 3, ARect.Top + 1, Value);
end;

procedure TImageIndexPropertyEditor.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
var
  ImgList: TCustomImageList;
begin
  ImgList := GetImageListAt(0);
  AHeight := ACanvas.TextHeight(Value) + 2;
  if Assigned(ImgList) and (ImgList.Height + 4 > AHeight) then
    AHeight := ImgList.Height + 4;
end;

procedure TImageIndexPropertyEditor.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
var
  ImgList: TCustomImageList;
begin
  ImgList := GetImageListAt(0);
  AWidth := ACanvas.TextWidth(Value) + 4;
  if Assigned(ImgList) then
    Inc(AWidth, ImgList.Width);
end;

{$IFDEF D10_4}
{ TImageNamePropertyEditor }

function TImageNamePropertyEditor.GetImageListAt(Index: Integer): TCustomImageList;
var
  LComponent: TPersistent;
begin
  Result := nil;
  LComponent := GetComponent(Index);
  if LComponent is TCustomStyledButton then
    Result := TCustomStyledButton(LComponent).Images
  else if LComponent is TCustomStyledGraphicButton then
    Result := TCustomStyledGraphicButton(LComponent).Images;
end;

function TImageNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TImageNamePropertyEditor.GetValues(Proc: TGetStrProc);
var
  ImgList: TCustomImageList;
  I: Integer;
  LImageName: TImageName;
  LImageIndex: TImageIndex;
begin
  ImgList := GetImageListAt(0);
  if Assigned(ImgList) and ImgList.IsImageNameAvailable then
  begin
    for I := 0 to ImgList.Count -1 do
    begin
      LImageName := '';
      LImageIndex := I;
      ImgList.CheckIndexAndName(LImageIndex, LImageName);
      Proc(LImageName);
    end;
  end;
end;

procedure TImageNamePropertyEditor.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  ImgList: TCustomImageList;
  X: Integer;
  LImageIndex: TImageIndex;
begin
  ImgList := GetImageListAt(0);
  ACanvas.FillRect(ARect);
  X := ARect.Left + 2;
  if Assigned(ImgList) then
  begin
    LImageIndex := ImgList.GetIndexByName(Value);
    ImgList.Draw(ACanvas, X, ARect.Top + 2, LImageIndex);
    Inc(X, ImgList.Width);
  end;
  ACanvas.TextOut(X + 3, ARect.Top + 1, Value);
end;

procedure TImageNamePropertyEditor.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
var
  ImgList: TCustomImageList;
begin
  ImgList := GetImageListAt(0);
  AHeight := ACanvas.TextHeight(Value) + 2;
  if Assigned(ImgList) and (ImgList.Height + 4 > AHeight) then
    AHeight := ImgList.Height + 4;
end;

procedure TImageNamePropertyEditor.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
var
  ImgList: TCustomImageList;
begin
  ImgList := GetImageListAt(0);
  AWidth := ACanvas.TextWidth(Value) + 4;
  if Assigned(ImgList) then
    Inc(AWidth, ImgList.Width);
end;
{$ENDIF}

procedure Register;
begin
  Classes.RegisterClasses(
    [TStyledToolButton]);

  RegisterComponents('Styled Components',
    [TStyledGraphicButton,
     TStyledSpeedButton,
     TStyledButton,
     TStyledBitBtn,
     TStyledToolbar,
     TStyledTaskDialog,
     TStyledDbNavigator,
     TStyledBindNavigator,
     TStyledButtonGroup,
     TStyledCategoryButtons]);

  RegisterPropertyEditor(TypeInfo(string),
    TStyledTaskDialog, 'Caption', TStringProperty);

  //Property Editor for StyleFamily
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledGraphicButton, 'StyleFamily', TStyledFamilyPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledSpeedButton, 'StyleFamily', TStyledFamilyPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledButton, 'StyleFamily', TStyledFamilyPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledBitBtn, 'StyleFamily', TStyledFamilyPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledToolbar, 'StyleFamily', TStyledFamilyPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledToolbutton, 'StyleFamily', TStyledFamilyPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledDbNavigator, 'StyleFamily', TStyledFamilyPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledBindNavigator, 'StyleFamily', TStyledFamilyPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledButtonGroup, 'StyleFamily', TStyledFamilyPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledGrpButtonItem, 'StyleFamily', TStyledFamilyPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledCategoryButtons, 'StyleFamily', TStyledFamilyPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledButtonItem, 'StyleFamily', TStyledFamilyPropertyEditor);

  //Property Editor for StyleClass
  RegisterPropertyEditor(TypeInfo(TStyledButtonClass),
    TStyledGraphicButton, 'StyleClass', TStyledClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonClass),
    TStyledSpeedButton, 'StyleClass', TStyledClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonClass),
    TStyledButton, 'StyleClass', TStyledClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonClass),
    TStyledBitBtn, 'StyleClass', TStyledClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonClass),
    TStyledToolbar, 'StyleClass', TStyledClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonClass),
    TStyledToolButton, 'StyleClass', TStyledClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonClass),
    TStyledDbNavigator, 'StyleClass', TStyledClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonClass),
    TStyledBindNavigator, 'StyleClass', TStyledClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonClass),
    TStyledButtonGroup, 'StyleClass', TStyledClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonClass),
    TStyledGrpButtonItem, 'StyleClass', TStyledClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonClass),
    TStyledCategoryButtons, 'StyleClass', TStyledClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonClass),
    TStyledButtonItem, 'StyleClass', TStyledClassPropertyEditor);

  //Property Editor for StyleAppearance
  RegisterPropertyEditor(TypeInfo(TStyledButtonAppearance),
    TStyledGraphicButton, 'StyleAppearance', TStyledAppearancePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonAppearance),
    TStyledSpeedButton, 'StyleAppearance', TStyledAppearancePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonAppearance),
    TStyledButton, 'StyleAppearance', TStyledAppearancePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonAppearance),
    TStyledBitBtn, 'StyleAppearance', TStyledAppearancePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonAppearance),
    TStyledToolbar, 'StyleAppearance', TStyledAppearancePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonAppearance),
    TStyledToolButton, 'StyleAppearance', TStyledAppearancePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonAppearance),
    TStyledDbNavigator, 'StyleAppearance', TStyledAppearancePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonAppearance),
    TStyledBindNavigator, 'StyleAppearance', TStyledAppearancePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonAppearance),
    TStyledButtonGroup, 'StyleAppearance', TStyledAppearancePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonAppearance),
    TStyledGrpButtonItem, 'StyleAppearance', TStyledAppearancePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonAppearance),
    TStyledCategoryButtons, 'StyleAppearance', TStyledAppearancePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonAppearance),
    TStyledButtonItem, 'StyleAppearance', TStyledAppearancePropertyEditor);

  //Property Editor for ImageIndex
  RegisterPropertyEditor(TypeInfo(System.UITypes.TImageIndex),
    TStyledGraphicButton, 'ImageIndex', TImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(System.UITypes.TImageIndex),
    TStyledSpeedButton, 'ImageIndex', TImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(System.UITypes.TImageIndex),
    TStyledButton, 'ImageIndex', TImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(System.UITypes.TImageIndex),
    TStyledBitBtn, 'ImageIndex', TImageIndexPropertyEditor);

  //Property Editor for StylusImageIndex
  RegisterPropertyEditor(TypeInfo(System.UITypes.TImageIndex),
    TStyledButton, 'StylusImageIndex', TImageIndexPropertyEditor);

  //Property Editor for HotImageIndex
  RegisterPropertyEditor(TypeInfo(System.UITypes.TImageIndex),
    TStyledGraphicButton, 'HotImageIndex', TImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(System.UITypes.TImageIndex),
    TStyledSpeedButton, 'HotImageIndex', TImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(System.UITypes.TImageIndex),
    TStyledButton, 'HotImageIndex', TImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(System.UITypes.TImageIndex),
    TStyledBitBtn, 'HotImageIndex', TImageIndexPropertyEditor);

{$IFDEF D10_4}
  //Property Editor for ImageName
  RegisterPropertyEditor(TypeInfo(System.UITypes.TImageName),
    TStyledGraphicButton, 'ImageName', TImageNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(System.UITypes.TImageName),
    TStyledSpeedButton, 'ImageName', TImageNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(System.UITypes.TImageName),
    TStyledButton, 'ImageName', TImageNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(System.UITypes.TImageName),
    TStyledBitBtn, 'ImageName', TImageNamePropertyEditor);

  //Property Editor for HotImageName
  RegisterPropertyEditor(TypeInfo(System.UITypes.TImageName),
    TStyledGraphicButton, 'HotImageName', TImageNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(System.UITypes.TImageName),
    TStyledSpeedButton, 'HotImageName', TImageNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(System.UITypes.TImageName),
    TStyledButton, 'HotImageName', TImageNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(System.UITypes.TImageName),
    TStyledBitBtn, 'HotImageName', TImageNamePropertyEditor);

  //Property Editor for StylusHotImageName
  RegisterPropertyEditor(TypeInfo(System.UITypes.TImageName),
    TStyledButton, 'StylusHotImageName', TImageNamePropertyEditor);
{$ENDIF}

  //TStyledButtonComponentEditor is shared by every Styled Component
  RegisterComponentEditor(TStyledGraphicButton, TStyledButtonComponentEditor);
  RegisterComponentEditor(TStyledSpeedButton, TStyledButtonComponentEditor);
  RegisterComponentEditor(TStyledButton, TStyledButtonComponentEditor);
  RegisterComponentEditor(TStyledBitBtn, TStyledButtonComponentEditor);
  RegisterComponentEditor(TStyledToolButton, TStyledButtonComponentEditor);

  //Register custom Components editors
  RegisterComponentEditor(TStyledToolbar, TStyledToolbarComponentEditor);
  RegisterComponentEditor(TStyledDbNavigator, TStyledNavigatorComponentEditor);
  RegisterComponentEditor(TStyledBindNavigator, TStyledNavigatorComponentEditor);
  RegisterComponentEditor(TStyledButtonGroup, TStyledButtonGroupComponentEditor);
  RegisterComponentEditor(TStyledCategoryButtons, TStyledCategoryButtonsComponentEditor);
  RegisterComponentEditor(TStyledTaskDialog, TStyledTaskDialogComponentEditor);

  //To auto add units
  RegisterSelectionEditor(TStyledGraphicButton, TStyledComponentSelection);
  RegisterSelectionEditor(TStyledSpeedButton, TStyledComponentSelection);
  RegisterSelectionEditor(TStyledButton, TStyledComponentSelection);
  RegisterSelectionEditor(TStyledBitBtn, TStyledComponentSelection);
  RegisterSelectionEditor(TStyledToolbar, TStyledComponentSelection);
  RegisterSelectionEditor(TStyledTaskDialog, TStyledComponentSelection);
  RegisterSelectionEditor(TStyledDbNavigator, TStyledComponentSelection);
  RegisterSelectionEditor(TStyledBindNavigator, TStyledComponentSelection);
  RegisterSelectionEditor(TStyledButtonGroup, TStyledComponentSelection);
  RegisterSelectionEditor(TStyledCategoryButtons, TStyledComponentSelection);
end;

end.
