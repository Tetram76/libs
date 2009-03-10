object FenGLMgrEdit: TFenGLMgrEdit
  Left = 79
  Top = 154
  Width = 641
  Height = 395
  Caption = 'Edition des listes g'#233'n'#233'riques'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelGlobal: TPanel
    Left = 0
    Top = 0
    Width = 633
    Height = 328
    Align = alClient
    BorderWidth = 3
    Caption = 'PanelGlobal'
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 206
      Top = 4
      Height = 293
    end
    object PanelDBNav: TPanel
      Left = 4
      Top = 297
      Width = 625
      Height = 27
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object DBNavPrincipal: TDBNavigator
        Left = 0
        Top = 0
        Width = 625
        Height = 27
        DataSource = DSElements
        Align = alClient
        TabOrder = 0
      end
    end
    object PanelItems: TPanel
      Left = 209
      Top = 4
      Width = 420
      Height = 293
      Align = alClient
      TabOrder = 1
      object DBGElements: TDBGrid
        Left = 1
        Top = 26
        Width = 418
        Height = 244
        Align = alClient
        DataSource = DSElements
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Verdana'
        TitleFont.Style = []
      end
      object FindPanelPrincipal: TFindPanel
        Left = 1
        Top = 1
        Width = 418
        Height = 25
        AllowCalcFields = False
        Caption = ' Rechercher un '#233'l'#233'ment '
        VisibleItems = [viButtons, viLabel]
        DataSet = TableElements
        SearchField = 'Item'
        ChangeIndex = False
        ComboBoxWidth = 85
        Align = alTop
        BevelOuter = bvLowered
        TabOrder = 1
        TabStop = True
      end
      object DBInfoPrincipal: TDBRecordCount
        Left = 1
        Top = 270
        Width = 418
        Height = 22
        DataSource = DSElements
        Align = alBottom
        BevelOuter = bvLowered
        TabOrder = 2
        RecordName = #233'l'#233'ment'
        PluralLetter = 's'
      end
    end
    object PanelGroupes: TPanel
      Left = 4
      Top = 4
      Width = 202
      Height = 293
      Align = alLeft
      TabOrder = 2
      object DBGGroupes: TDBGrid
        Left = 1
        Top = 26
        Width = 200
        Height = 244
        Align = alClient
        DataSource = DSGroupes
        Options = [dgIndicator, dgColumnResize, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete]
        ReadOnly = True
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Verdana'
        TitleFont.Style = []
      end
      object PanelTitreGroupe: TPanel
        Left = 1
        Top = 1
        Width = 200
        Height = 25
        Align = alTop
        BevelOuter = bvLowered
        Caption = 'Groupes'
        TabOrder = 1
      end
      object DBRecordCount1: TDBRecordCount
        Left = 1
        Top = 270
        Width = 200
        Height = 22
        DataSource = DSGroupes
        Align = alBottom
        BevelOuter = bvLowered
        TabOrder = 2
        RecordName = 'groupe'
        PluralLetter = 's'
      end
    end
  end
  object PanelBoutons: TPanel
    Left = 0
    Top = 328
    Width = 633
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object DBNavBarrePrincipal: TDBNavBarre
      Left = 0
      Top = 0
      Width = 538
      Height = 40
      Align = alClient
      BevelOuter = bvLowered
      NavMode = nmSingleDatasource
      DataSource = DSElements
      BtnSpacing = 6
      Spacing = 2
      MarginHeight = 3
      MarginWidth = 3
      DeleteMessage = 'Effacer l'#39'enregistrement ?'
      FocusControl = DBGElements
    end
    object PanelBoutonFermer: TPanel
      Left = 538
      Top = 0
      Width = 95
      Height = 40
      Align = alRight
      BevelOuter = bvLowered
      TabOrder = 1
      object FermerBtn: TBitBtn
        Left = 3
        Top = 3
        Width = 89
        Height = 34
        Caption = '&Fermer'
        TabOrder = 0
        OnClick = FermerBtnClick
        Glyph.Data = {
          36060000424D3606000000000000360000002800000020000000100000000100
          1800000000000006000000000000000000000000000000000000007F7F007F7F
          0000000000000000000000000000000000000000000000000000000000000000
          00007F7F007F7F007F7F007F7F007F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F
          7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7F7F7F7F007F7FFFFFFF007F7F00
          7F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF7F7F7F007F7FFFFFFF00
          7F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F7F7F7F007F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFFFFFFFF007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00FFFF00000000FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7F7F
          7F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7FFFFFFF7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007FFFFF0000007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF7F7F7FFFFFFF7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007FFFFF00FFFF0000000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF7F7F7F7F7F7F7F7F7FFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          0000000000000000000000000000000000000000000000000000000000000000
          00007F7F007F7F007F7F007F7F007F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F
          7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F007F7F007F7F007F7F}
        NumGlyphs = 2
      end
    end
  end
  object TableElements: TTable
    BeforeInsert = TableElementsBeforeInsert
    BeforeDelete = TableElementsBeforeDelete
    IndexFieldNames = 'GroupName'
    MasterFields = 'GroupName'
    MasterSource = DSGroupes
    TableName = 'GItems'
    Left = 236
    Top = 68
    object TableElementsGroupName: TStringField
      DisplayLabel = 'Nom de groupe'
      FieldName = 'GroupName'
      Visible = False
      Size = 50
    end
    object TableElementsOrder: TIntegerField
      DisplayLabel = 'Ordre'
      DisplayWidth = 6
      FieldName = 'Order'
    end
    object TableElementsItem: TStringField
      DisplayLabel = 'El'#233'ment'
      DisplayWidth = 30
      FieldName = 'Item'
      Size = 255
    end
    object TableElementsValue: TStringField
      DisplayLabel = 'Valeur'
      DisplayWidth = 31
      FieldName = 'Value'
      OnGetText = TableElementsValueGetText
      Size = 255
    end
  end
  object DSElements: TDataSource
    DataSet = TableElements
    Left = 236
    Top = 104
  end
  object TableGroupes: TTable
    TableName = 'GGroups'
    Left = 88
    Top = 68
    object TableGroupesGroupName: TStringField
      DisplayLabel = 'Nom de groupe'
      FieldName = 'GroupName'
      Size = 50
    end
    object TableGroupesAllowAdd: TIntegerField
      FieldName = 'AllowAdd'
      Visible = False
    end
    object TableGroupesAllowDelete: TIntegerField
      FieldName = 'AllowDelete'
      Visible = False
    end
    object TableGroupesAllowValue: TIntegerField
      FieldName = 'AllowValue'
      Visible = False
    end
    object TableGroupesAllowOrder: TIntegerField
      FieldName = 'AllowOrder'
      Visible = False
    end
    object TableGroupesValueName: TStringField
      FieldName = 'ValueName'
      Visible = False
    end
    object TableGroupesValueUnit: TStringField
      FieldName = 'ValueUnit'
      Visible = False
      Size = 5
    end
    object TableGroupesItemReadOnly: TIntegerField
      FieldName = 'ItemReadOnly'
      Visible = False
    end
  end
  object DSGroupes: TDataSource
    AutoEdit = False
    DataSet = TableGroupes
    OnDataChange = DSGroupesDataChange
    Left = 88
    Top = 104
  end
end
