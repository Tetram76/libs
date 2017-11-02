unit FGCPpanl;

interface

{$I GrizzlyDefine.INC}

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Buttons, TypInfo
  {$IFDEF GZ_D6}
  ,DesignIntf, DesignEditors, Contnrs
  {$ELSE}
  ,DsgnIntf
  {$ENDIF};

type
  TcpPanelDlg = class(TForm)
    BtnEffacer: TBitBtn;
    BtnClick: TBitBtn;
    BtnCancel: TBitBtn;
  private
    { Déclarations private }
  public
    { Déclarations public }
  end;

var
  cpPanelDlg: TcpPanelDlg;


Procedure Register;

implementation

{$R *.DFM}

Type
	TFGPanelComponentProperty = Class(TComponentEditor)
  Private
    {$IFDEF GZ_D6}
  	Procedure DoForEach(const PropertyEditor : IProperty);
    {$ELSE}
  	Procedure DoForEach(PropertyEditor : TPropertyEditor);
    {$ENDIF}
  Public
  	Function GetVerbCount : Integer; override;
    Function GetVerb(Index : Integer) : String; Override;
    Procedure ExecuteVerb(Index : Integer); Override;
    Procedure ExecuteOnClick;
  End;

Function TFGPanelComponentProperty.GetVerbCount : Integer;
Begin
	Result:=1;
End;

Function TFGPanelComponentProperty.GetVerb(Index : Integer) : String;
Begin
	If Index=0 then
  	result:='Editeur de Panel'
End;

Procedure TFGPanelComponentProperty.ExecuteVerb(Index : Integer);
Var
	Res : TModalResult;
Begin
	If Index=0 then
  Begin
  	cpPanelDlg:=TcpPanelDlg.Create(Nil);
    Try
	  	Res:=cpPanelDlg.ShowModal;
	  	If Res=mrYes then
				(Component as TPanel).Caption:=''
	    Else If Res=mrNo then
				ExecuteOnClick;
    Finally
    	cpPanelDlg.Free;
    End;
	End;
End;

{$IFDEF GZ_D6}
Procedure TFGPanelComponentProperty.ExecuteOnClick;
Var
	Components : IDesignerSelections;
Begin
	Components := TDesignerSelections.Create;
  Components.Add(Component);
  GetComponentProperties(Components,[tkMethod], Designer, DoForEach);
End;
{$ELSE}
Procedure TFGPanelComponentProperty.ExecuteOnClick;
Var
	Components : TDesignerSelectionList;
Begin
	Components:=TDesignerSelectionList.Create;
  try
    Components.Add(Component);
    GetComponentProperties(Components,[tkMethod], Designer, DoForEach);
  finally
    Components.Free;
  end;
End;
{$ENDIF}

{$IFDEF GZ_D6}
Procedure TFGPanelComponentProperty.DoForEach(const PropertyEditor : IProperty);
Var
	PropName : String;
Begin
	PropName:=PropertyEditor.GetName;
  If CompareText(PropName,'ONCLICK')=0 then
		PropertyEditor.Edit;
End;
{$ELSE}
Procedure TFGPanelComponentProperty.DoForEach(PropertyEditor : TPropertyEditor);
Var
	PropName : String;
Begin
	PropName:=PropertyEditor.GetName;
  If CompareText(PropName,'ONCLICK')=0 then
		PropertyEditor.Edit;
  PropertyEditor.Free;
End;
{$ENDIF}

Procedure Register;
Begin
	RegisterComponentEditor(TPanel, TFGPanelComponentProperty);
End;

end.
