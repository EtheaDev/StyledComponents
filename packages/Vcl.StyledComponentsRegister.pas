{******************************************************************************}
{                                                                              }
{       StyledComponents: a set of Styled VCL Component                        }
{                                                                              }
{       Copyright (c) 2022-2023 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
{                                                                              }
{       https://github.com/EtheaDev/StyledComponents                           }
{                                                                              }
{******************************************************************************}
unit Vcl.StyledComponentsRegister;

interface

uses
  Classes
  , DesignIntf
  , Designer
  , DesignEditors
  , VCLEditors
  , Vcl.Controls
  , Vcl.ComCtrls
  , Vcl.StyledButton
  , Vcl.StyledToolbar
  , Vcl.StyledDbNavigator
  , Vcl.ButtonStylesAttributes
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

  TStyledButtonComponentEditor = class (TComponentEditor)
  private
    function GetButton: TControl;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

  TStyledToolbarComponentEditor = class (TComponentEditor)
  private
    function GetToolbar: TStyledToolbar;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TStyledDbNavigatorComponentEditor = class (TComponentEditor)
  private
    function GetDbNavigator: TStyledDbNavigator;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TStyledButtonsSelection = class (TSelectionEditor, ISelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  Vcl.StyledTaskDialog
  , Vcl.StandardButtonStyles
  , Vcl.BootstrapButtonStyles
  , Vcl.AngularButtonStyles
  , Vcl.StyledButtonEditorUnit
  , Vcl.StyledCmpStrUtils
  , Vcl.DbCtrls
  , System.Contnrs
  , Winapi.ShellAPI
  , Winapi.Windows
  ;


function GetComponentFamilyClass(const AComponent: TPersistent;
  out AButtonFamily: TButtonFamily): boolean;
var
  LFamily: TStyledButtonFamily;
begin
  LFamily := '';
  if AComponent is TStyledGraphicButton then
    LFamily := TStyledGraphicButton(AComponent).StyleFamily
  else if AComponent is TStyledButton then
    LFamily := TStyledButton(AComponent).StyleFamily
  else if AComponent is TStyledToolbar then
    LFamily := TStyledToolbar(AComponent).StyleFamily
  else if AComponent is TStyledDbNavigator then
    LFamily := TStyledDbNavigator(AComponent).StyleFamily;
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
  if LComponent is TControl then
    Result := TStyledGraphicButton(LComponent);
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

{ TStyledButtonsSelection }
procedure TStyledButtonsSelection.RequiresUnits(Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);
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

{ TStyledDbNavigatorComponentEditor }

procedure TStyledDbNavigatorComponentEditor.ExecuteVerb(Index: Integer);
var
  LDbNavigator: TStyledDbNavigator;
  LNavButton: TStyledNavButton;
begin
  inherited;
  if Index = 0 then
  begin
    LDbNavigator := GetDbNavigator;
    LNavButton := LDbNavigator.Buttons[nbFirst];
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
  end
  else if Index = 1 then
  ShellExecute(0, 'open',
    PChar(GetProjectURL), nil, nil, SW_SHOWNORMAL);
end;

function TStyledDbNavigatorComponentEditor.GetDbNavigator: TStyledDbNavigator;
var
  LComponent: TPersistent;
begin
  Result := nil;
  LComponent := GetComponent;
  if LComponent is TStyledDbNavigator then
    Result := TStyledDbNavigator(LComponent);
end;

function TStyledDbNavigatorComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Styled DbNavigator Editor...'
  else if Index = 1 then
    Result := 'Project page on GitHub...';
end;

function TStyledDbNavigatorComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

procedure Register;
begin
  Classes.RegisterClass(TStyledToolButton);

  RegisterComponents('Styled Components',
    [TStyledGraphicButton,
     TStyledButton,
     TStyledToolbar,
     TStyledTaskDialog,
     TStyledDbNavigator]);

  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledGraphicButton, 'StyleFamily', TStyledFamilyPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledButton, 'StyleFamily', TStyledFamilyPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledToolbar, 'StyleFamily', TStyledFamilyPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledDbNavigator, 'StyleFamily', TStyledFamilyPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TStyledButtonClass),
    TStyledGraphicButton, 'StyleClass', TStyledClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledButton, 'StyleClass', TStyledClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledToolbar, 'StyleClass', TStyledClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledDbNavigator, 'StyleClass', TStyledClassPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TStyledButtonAppearance),
    TStyledGraphicButton, 'StyleAppearance', TStyledAppearancePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledButton, 'StyleAppearance', TStyledAppearancePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledToolbar, 'StyleAppearance', TStyledAppearancePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledDbNavigator, 'StyleAppearance', TStyledAppearancePropertyEditor);

  RegisterComponentEditor(TStyledGraphicButton, TStyledButtonComponentEditor);
  RegisterComponentEditor(TStyledButton, TStyledButtonComponentEditor);
  RegisterComponentEditor(TStyledToolButton, TStyledButtonComponentEditor);
  RegisterComponentEditor(TStyledToolbar, TStyledToolbarComponentEditor);
  RegisterComponentEditor(TStyledDbNavigator, TStyledDbNavigatorComponentEditor);

  //To auto add units
  RegisterSelectionEditor(TStyledGraphicButton, TStyledButtonsSelection);
end;

end.
