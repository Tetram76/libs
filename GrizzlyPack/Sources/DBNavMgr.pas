unit DBNavMgr;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DBNavBtn;

type
  TDBNavManager = class(TComponent)
  private
    function GetInsert: string;
    procedure SetInsert(Str: string);
    function GetEdit: string;
    procedure SetEdit(Str: string);
    function GetDelete: string;
    procedure SetDelete(Str: string);
    function GetPost: string;
    procedure SetPost(Str: string);
    function GetCancel: string;
    procedure SetCancel(Str: string);
    function GetLabel(Idx: Integer): string;
    {}
    function GetInsertH: string;
    procedure SetInsertH(Str: string);
    function GetEditH: string;
    procedure SetEditH(Str: string);
    function GetDeleteH: string;
    procedure SetDeleteH(Str: string);
    function GetPostH: string;
    procedure SetPostH(Str: string);
    function GetCancelH: string;
    procedure SetCancelH(Str: string);
    function GetHint(Idx: Integer): string;
  protected
  public
    property Labels[Idx: Integer]: string read GetLabel;
    property Hints[Idx: Integer]: string read GetHint;
  published
    property InsertLabel: string read GetInsert write SetInsert;
    property EditLabel: string read GetEdit write SetEdit;
    property DeleteLabel: string read GetDelete write SetDelete;
    property PostLabel: string read GetPost write SetPost;
    property CancelLabel: string read GetCancel write SetCancel;
    {}
    property InsertHint: string read GetInsertH write SetInsertH;
    property EditHint: string read GetEditH write SetEditH;
    property DeleteHint: string read GetDeleteH write SetDeleteH;
    property PostHint: string read GetPostH write SetPostH;
    property CancelHint: string read GetCancelH write SetCancelH;
  end;

  function StandardLabel(Idx: Integer): string;
  function StandardHint(Idx: Integer): string;

var
  InsertLabel: string;
  EditLabel: string;
  DeleteLabel: string;
  PostLabel: string;
  CancelLabel: string;
  {}
  InsertHint: string;
  EditHint: string;
  DeleteHint: string;
  PostHint: string;
  CancelHint: string;

implementation

uses UFGDBCtl;

function TDBNavManager.GetInsert: string;
begin
  Result:= DBNavMgr.InsertLabel;
end;

procedure TDBNavManager.SetInsert(Str: string);
begin
  DBNavMgr.InsertLabel:= Str;
end;

function TDBNavManager.GetEdit: string;
begin
  Result:= DBNavMgr.EditLabel;
end;

procedure TDBNavManager.SetEdit(Str: string);
begin
  DBNavMgr.EditLabel:= Str;
end;

function TDBNavManager.GetDelete: string;
begin
  Result:= DBNavMgr.DeleteLabel;
end;

procedure TDBNavManager.SetDelete(Str: string);
begin
  DBNavMgr.DeleteLabel:= Str;
end;

function TDBNavManager.GetPost: string;
begin
  Result:= DBNavMgr.PostLabel;
end;

procedure TDBNavManager.SetPost(Str: string);
begin
  DBNavMgr.PostLabel:= Str;
end;

function TDBNavManager.GetCancel: string;
begin
  Result:= DBNavMgr.CancelLabel;
end;

procedure TDBNavManager.SetCancel(Str: string);
begin
  DBNavMgr.CancelLabel:= Str;
end;

function TDBNavManager.GetLabel(Idx: Integer): string;
begin
  Result:= StandardLabel(Idx);
end;

function TDBNavManager.GetInsertH: string;
begin
  Result:= DBNavMgr.InsertHint;
end;

procedure TDBNavManager.SetInsertH(Str: string);
begin
  DBNavMgr.InsertHint:= Str;
end;

function TDBNavManager.GetEditH: string;
begin
  Result:= DBNavMgr.EditHint;
end;

procedure TDBNavManager.SetEditH(Str: string);
begin
  DBNavMgr.EditHint:= Str;
end;

function TDBNavManager.GetDeleteH: string;
begin
  Result:= DBNavMgr.DeleteHint;
end;

procedure TDBNavManager.SetDeleteH(Str: string);
begin
  DBNavMgr.DeleteHint:= Str;
end;

function TDBNavManager.GetPostH: string;
begin
  Result:= DBNavMgr.PostHint;
end;

procedure TDBNavManager.SetPostH(Str: string);
begin
  DBNavMgr.PostHint:= Str;
end;

function TDBNavManager.GetCancelH: string;
begin
  Result:= DBNavMgr.CancelHint;
end;

procedure TDBNavManager.SetCancelH(Str: string);
begin
  DBNavMgr.CancelHint:= Str;
end;

function TDBNavManager.GetHint(Idx: Integer): string;
begin
  Result:= StandardHint(Idx);
end;

function StandardLabel(Idx: Integer): string;
begin
  Result:= '';
  case Idx of
    0: Result:= InsertLabel;
    1: Result:= EditLabel;
    2: Result:= DeleteLabel;
    3: Result:= PostLabel;
    4: Result:= CancelLabel;
  end;
end;

function StandardHint(Idx: Integer): string;
begin
  Result:= '';
  case Idx of
    0: Result:= InsertHint;
    1: Result:= EditHint;
    2: Result:= DeleteHint;
    3: Result:= PostHint;
    4: Result:= CancelHint;
  end;
end;

initialization
begin
  InsertLabel:= msgBtnInsert;
  EditLabel:= msgBtnEdit;
  DeleteLabel:= msgBtnDelete;
  PostLabel:= msgBtnPost;
  CancelLabel:= msgBtnCancel;
  {}
  InsertHint:= hintBtnInsert;
  EditHint:= hintBtnEdit;
  DeleteHint:= hintBtnDelete;
  PostHint:= hintBtnPost;
  CancelHint:= hintBtnCancel;
end;

end.
