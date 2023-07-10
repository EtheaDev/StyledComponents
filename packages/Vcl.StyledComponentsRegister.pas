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
  , Vcl.StyledButton
  , Vcl.StyledToolbar
  ;

Type
  TButtonFamilyPropertyEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TButtonClassPropertyEditor = class(TStringProperty)
  protected
    function GetButton: TStyledGraphicButton;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TButtonAppearancePropertyEditor = class(TStringProperty)
  protected
    function GetButton: TStyledGraphicButton;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TButtonPropertyEditor = class(TPropertyEditor)
  protected
    function GetButton: TStyledGraphicButton;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetValue: string; override; abstract;
  end;

  TStyledGraphicButtonComponentEditor = class (TComponentEditor)
  private
    function GetButton: TStyledGraphicButton;
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

  TStyledButtonsSelection = class (TSelectionEditor, ISelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  Vcl.StyledTaskDialog
  , Vcl.ButtonStylesAttributes
  , Vcl.StandardButtonStyles
  , Vcl.BootstrapButtonStyles
  , Vcl.AngularButtonStyles
  , Vcl.StyledButtonEditorUnit
  , Vcl.StyledCmpStrUtils
  , System.Contnrs
  , Winapi.ShellAPI
  , Winapi.Windows
  ;

{ TButtonFamilyPropertyEditor }

function TButtonFamilyPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect];
end;

procedure TButtonFamilyPropertyEditor.GetValues(Proc: TGetStrProc);
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


{ TButtonClassPropertyEditor }

function TButtonClassPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect];
end;

function TButtonClassPropertyEditor.GetButton: TStyledGraphicButton;
var
  LComponent: TPersistent;
begin
  Result := nil;
  LComponent := GetComponent(0);
  if LComponent is TStyledGraphicButton then
    Result := TStyledGraphicButton(LComponent);
end;

procedure TButtonClassPropertyEditor.GetValues(Proc: TGetStrProc);
var
  LClasses: TButtonClasses;
  I: Integer;
  LButton: TStyledGraphicButton;
  LFamily: TButtonFamily;
begin
  LButton := GetButton;
  if Assigned(LButton) then
  begin
    LFamily := GetButtonFamilyClass(LButton.StyleFamily);
    LClasses := GetButtonClasses(LFamily);
    if Assigned(LClasses) then
    begin
      for I := Low(LClasses) to High(LClasses) do
        Proc(LClasses[I]);
    end;
  end;
end;

{ TButtonAppearancePropertyEditor }

function TButtonAppearancePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect];
end;

function TButtonAppearancePropertyEditor.GetButton: TStyledGraphicButton;
var
  LComponent: TPersistent;
begin
  Result := nil;
  LComponent := GetComponent(0);
  if LComponent is TStyledGraphicButton then
    Result := TStyledGraphicButton(LComponent);
end;

procedure TButtonAppearancePropertyEditor.GetValues(Proc: TGetStrProc);
var
  LAppearances: TButtonAppearances;
  I: Integer;
  LButton: TStyledGraphicButton;
  LFamily: TButtonFamily;
begin
  LButton := GetButton;
  if Assigned(LButton) then
  begin
    LFamily := GetButtonFamilyClass(LButton.StyleFamily);
    LAppearances := GetButtonAppearances(LFamily);
    if Assigned(LAppearances) then
    begin
      for I := Low(LAppearances) to High(LAppearances) do
        Proc(LAppearances[I]);
    end;
  end;
end;


{ TStyledGraphicButtonComponentEditor }

function TStyledGraphicButtonComponentEditor.GetButton: TStyledGraphicButton;
var
  LComponent: TPersistent;
begin
  Result := nil;
  LComponent := GetComponent;
  if LComponent is TStyledGraphicButton then
    Result := TStyledGraphicButton(LComponent);
end;

procedure TStyledGraphicButtonComponentEditor.Edit;
begin
  inherited;
end;

procedure TStyledGraphicButtonComponentEditor.ExecuteVerb(Index: Integer);
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

function TStyledGraphicButtonComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Styled Button Editor...'
  else if Index = 1 then
    Result := 'Project page on GitHub...';
end;

function TStyledGraphicButtonComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TButtonPropertyEditor }

procedure TButtonPropertyEditor.Edit;
var
  Component : TComponent;
begin
  Component := TComponent(GetComponent(0));
  if Component is TStyledGraphicButton then
  begin
    if EditStyledButton(TStyledGraphicButton(Component)) then
      Designer.Modified;
  end;
end;

function TButtonPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TButtonPropertyEditor.GetButton: TStyledGraphicButton;
var
  Component : TComponent;
begin
  Component := TComponent(GetComponent(0));
  if Component is TStyledGraphicButton then
    Result := TStyledGraphicButton(Component)
  else
    Result := nil;
end;

{ TStyledButtonsSelection }
procedure TStyledButtonsSelection.RequiresUnits(Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);
  //Units added to source to manage styles of buttons
  proc('Vcl.ButtonStylesAttributes');
  proc('Vcl.StandardButtonStyles');
  proc('Vcl.AngularButtonStyles');
  proc('Vcl.BootstrapButtonStyles');
  proc('Vcl.ColorButtonStyles');
end;


{ TStyledToolbarComponentEditor }

procedure TStyledToolbarComponentEditor.ExecuteVerb(Index: Integer);
var
  LToolbar: TStyledToolbar;
  LNewButton: TStyledToolButton;
begin
  inherited;
  if Index = 0 then
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
        Designer.Modified;
      end;
    finally
      LNewButton.Free;
    end;
  end
  else if Index = 1 then //Add Button
  begin
    if GetToolbar.NewButton(LNewButton, tbsStyledButton) then
    begin
      //Set unique Name to the Button created
      LNewButton.Name := Designer.UniqueName('StyledToolButton');
      LNewButton.Caption := LNewButton.Name;
      //Select the button into Designer
      Designer.SelectComponent(LNewButton);
      Designer.Modified;
    end;
  end
  else if Index = 2 then //Add Separator
  begin
    if GetToolbar.NewButton(LNewButton, tbsStyledSeparator) then
    begin
      //Set unique Name to the Button created
      LNewButton.Name := Designer.UniqueName('StyledToolButton');
      LNewButton.Caption := LNewButton.Name;
      //Select the button into Designer
      Designer.SelectComponent(LNewButton);
      Designer.Modified;
    end;
  end
  else if Index = 3 then
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

procedure Register;
begin
  Classes.RegisterClass(TStyledToolButton);

  RegisterComponents('Styled Components',
    [TStyledGraphicButton,
     TStyledButton,
     TStyledToolbar,
     TStyledTaskDialog]);

  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledGraphicButton, 'StyleFamily', TButtonFamilyPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TStyledButtonClass),
    TStyledGraphicButton, 'StyleClass', TButtonClassPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TStyledButtonAppearance),
    TStyledGraphicButton, 'StyleAppearance', TButtonAppearancePropertyEditor);

  RegisterComponentEditor(TStyledGraphicButton,
    TStyledGraphicButtonComponentEditor);

  RegisterComponentEditor(TStyledToolbar,
    TStyledToolbarComponentEditor);

  //To auto add units
  RegisterSelectionEditor(TStyledGraphicButton, TStyledButtonsSelection);
end;

end.
