object GLFenAppGenericList: TGLFenAppGenericList
  Left = 108
  Top = 89
  Width = 641
  Height = 460
  Caption = 'Listes G'#233'n'#233'riques'
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
  PixelsPerInch = 96
  TextHeight = 13
  object PanelGlobal: TPanel
    Left = 0
    Top = 0
    Width = 633
    Height = 393
    Align = alClient
    BorderWidth = 3
    Caption = 'PanelGlobal'
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 4
      Top = 160
      Width = 625
      Height = 3
      Cursor = crVSplit
      Align = alTop
    end
    object DBGElements: TDBGrid
      Left = 4
      Top = 188
      Width = 625
      Height = 174
      Align = alClient
      DataSource = DSElements
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Verdana'
      TitleFont.Style = []
    end
    object PanelDBNav: TPanel
      Left = 4
      Top = 362
      Width = 625
      Height = 27
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object DBNavPrincipal: TDBNavigator
        Left = 0
        Top = 0
        Width = 490
        Height = 27
        DataSource = DSElements
        Align = alClient
        TabOrder = 0
      end
      object DBInfoPrincipal: TDBRecordCount
        Left = 490
        Top = 0
        Width = 135
        Height = 27
        DataSource = DSElements
        Align = alRight
        BevelOuter = bvLowered
        TabOrder = 1
        RecordName = 'enregistrement'
        PluralLetter = 's'
      end
    end
    object FindPanelPrincipal: TFindPanel
      Left = 4
      Top = 163
      Width = 625
      Height = 25
      AllowCalcFields = False
      Caption = ' Rechercher l'#39#233'l'#233'ment '
      VisibleItems = [viButtons, viComboBox, viLabel]
      DataSet = TableElements
      SearchField = 'Item'
      ChangeIndex = False
      ComboBoxWidth = 85
      Align = alTop
      TabOrder = 2
      TabStop = True
    end
    object DBGGroupes: TDBGrid
      Left = 4
      Top = 36
      Width = 625
      Height = 124
      Align = alTop
      DataSource = DSGroupes
      Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete]
      TabOrder = 3
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Verdana'
      TitleFont.Style = []
    end
    object Panel1: TPanel
      Left = 4
      Top = 4
      Width = 625
      Height = 32
      Align = alTop
      TabOrder = 4
      object BtnLien: TSpeedButton
        Left = 4
        Top = 4
        Width = 25
        Height = 25
        Hint = 'Lien ma'#238'tre/d'#233'tail'
        AllowAllUp = True
        GroupIndex = 1
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000003
          333333333F777773FF333333008888800333333377333F3773F3333077870787
          7033333733337F33373F3308888707888803337F33337F33337F330777880887
          7703337F33337FF3337F3308888000888803337F333777F3337F330777700077
          7703337F33377733337FB3088888888888033373FFFFFFFFFF733B3000000000
          0033333777777777773333BBBB3333080333333333F3337F7F33BBBB707BB308
          03333333373F337F7F3333BB08033308033333337F7F337F7F333B3B08033308
          033333337F73FF737F33B33B778000877333333373F777337333333B30888880
          33333333373FFFF73333333B3300000333333333337777733333}
        NumGlyphs = 2
        OnClick = BtnLienClick
      end
      object LabelTables: TLabel
        Left = 156
        Top = 8
        Width = 4
        Height = 13
      end
      object Button1: TButton
        Left = 32
        Top = 4
        Width = 113
        Height = 25
        Caption = 'Base de donn'#233'es'
        TabOrder = 0
        OnClick = Button1Click
      end
    end
  end
  object PanelBoutons: TPanel
    Left = 0
    Top = 393
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
      NavMode = nmMultiDatasource
      DataSource = DSElements
      BtnSpacing = 6
      Spacing = 2
      MarginHeight = 3
      MarginWidth = 3
      DeleteMessage = 'Effacer l'#39'enregistrement ?'
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
    IndexFieldNames = 'GroupName'
    MasterFields = 'GroupName'
    MasterSource = DSLienGroupes
    TableName = 'GItems'
    Left = 176
    Top = 60
    object TableElementsGroupName: TStringField
      DisplayLabel = 'Nom de groupe'
      DisplayWidth = 25
      FieldName = 'GroupName'
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
      DisplayWidth = 25
      FieldName = 'Value'
      OnGetText = TableElementsValueGetText
      Size = 255
    end
  end
  object DSElements: TDataSource
    DataSet = TableElements
    Left = 176
    Top = 100
  end
  object TableGroupes: TTable
    TableName = 'GGroups'
    Left = 64
    Top = 60
    object TableGroupesGroupName: TStringField
      DisplayLabel = 'Nom de groupe'
      DisplayWidth = 20
      FieldName = 'GroupName'
      Size = 50
    end
    object TableGroupesAllowAdd: TIntegerField
      DisplayLabel = 'Add'
      FieldName = 'AllowAdd'
      OnGetText = TableGroupesAllowAddGetText
    end
    object TableGroupesAllowDelete: TIntegerField
      DisplayLabel = 'Delete'
      FieldName = 'AllowDelete'
      OnGetText = TableGroupesAllowAddGetText
    end
    object TableGroupesAllowOrder: TIntegerField
      DisplayLabel = 'Order'
      FieldName = 'AllowOrder'
      OnGetText = TableGroupesAllowAddGetText
    end
    object TableGroupesAllowValue: TIntegerField
      DisplayLabel = 'Value'
      FieldName = 'AllowValue'
      OnGetText = TableGroupesAllowAddGetText
    end
    object TableGroupesItemReadOnly: TIntegerField
      DisplayLabel = 'Item ReadOnly'
      FieldName = 'ItemReadOnly'
      OnGetText = TableGroupesAllowAddGetText
    end
    object TableGroupesValueName: TStringField
      FieldName = 'ValueName'
    end
    object TableGroupesValueUnit: TStringField
      FieldName = 'ValueUnit'
      Size = 5
    end
  end
  object DSGroupes: TDataSource
    DataSet = TableGroupes
    OnDataChange = DSGroupesDataChange
    Left = 64
    Top = 100
  end
  object DSLienGroupes: TDataSource
    DataSet = TableGroupes
    Left = 104
    Top = 60
  end
  object DatabaseNameDialog1: TDatabaseNameDialog
    Left = 140
    Top = 60
  end
end
