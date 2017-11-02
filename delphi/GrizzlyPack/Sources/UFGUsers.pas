unit UFGUsers;

{This unit contains all the const that will help you
to localize UserManager components}

interface

Const
  msgNoDatasetDefined = 'Dataset absent, pas d''identification possible !';
  msgNoAccessDatasetDefined = 'AccessDataset absent, utilisation des niveaux d''accès impossible !';
  msgBadPasswordEmptyString = 'Mot de passe incorrect (chaine vide)';
  msgGoodPassword = 'Mot de passe correct';
  msgBadPassword = 'Mot de passe incorrect';
  msgTriesSinceLastGood = '%s essais infructueux depuis le dernier réussi';
  msgUnknownUser = 'Utilisateur inconnu';
  msgIdentification = 'Identification';
  msgPassword = 'Mot de passe';
  msgSupervisorPassword = 'Mot de passe Superviseur';
  msgCantModifySupervisorLevel = 'Le niveau du superviseur n''est pas modifiable';
  msgCantSaveWithoutPassword = 'Impossible de sauver si le mot de passe est indéfini !';
  msgCantDeleteSupervisor = 'Superviseur non effacable !';
  msgIdApplication = 'Application : %s';
  msgBadConfirmation = 'La confirmation ne correspond pas';
  msgUserShortName = 'Identifiant de l''utilisateur';
  msgUserPassword = 'Mot de passe';
  msgOk = 'OK';
  msgCancel = 'Annuler';
  msgModifyPassword = 'Modifier un mot de passe';
  msgOldPassword = 'Ancien mot de passe';
  msgNewPassword = 'Nouveau mot de passe';
  msgConfirmationPassword = 'Confirmation';
  msgUsersFile = 'Fichier des Utilisateurs';
  msgLongName = 'N&om de l''utilisateur';
  msgShortName = '&Identifiant de l''utilisateur';
  msgUserLevel = 'Ni&veau de l''utilisateur (1, 2 ou 3)';
  msgChangePassword = '&Changer le mot de passe';
  msgChangeLevel = 'N&iveau...';
  msgClose = '&Fermer';
  msgYes = '&Oui';
  msgNo = '&Non';

  msgAccessElemDoesNotExist = 'L''élément d''accès "%s" ' + 'n''existe pas !';
  msgAccessDenied = 'Droits d''accès insuffisants pour accéder à : %s';

    { Access Rights messages }
  msgAccessKindLevel = 'Par n&iveau';
  msgAccessKindUser = 'Par &utilisateur';
  msgAccessKindBoth = 'Par les &deux';
  msgEnterElement = 'Entrer dans un élément de l''application';
  msgElementName = 'Nom de l''élément';

    { Window EditElements }
  msgAccessManagement = 'Gestion des droits d''accès';
  msgPerLevel = 'Par n&iveau';
  msgShortLevel = 'N';
  msgPerUser = 'Par &utilisateur';
  msgShortUser = 'U';
  msgPerBoth = 'Par les &deux';
  msgShortBoth = 'D';
  msgShortElemName = 'Nom élément';
  msgRequiredAccessRight = 'Niveau d''accès requis';
  msgRequiredPassword = 'Mot de passe requis';
  msgBtnModifyPassword = 'Modifier mot de &passe';
  msgShortUsersFile = 'Fichier utilisateurs';
  msgUsersRights = 'Droits utilisateurs';
  msgAccessFieldElem = 'Elément';
  msgAccessFieldAccessKind = 'Type';
  msgAccessFieldLevel = 'Niveau';
  msgAccessRightIncorrect = 'Le droit d''accès requis doit être compris entre 1 et 4 !';
  msgNoUserLimitedElement = 'Il n''y a aucun élément ayant un droit d''accès sur les ' +
    'utilisateurs !';

    { CrossEditing }
  msgUsers = 'Utilisateurs';
  msgUser = 'Utilisateur';
  msgSelectedElem = 'Elément sélectionné';
  msgAccessEnabled = 'Accès autorisé';
  msgSearch = 'Rechercher';
  msgAUser = 'Un utilisateur';
  msgAnElem = 'Un élément';
  msgTypeSearchString = 'Saisissez la chaîne à rechercher';
  

implementation

end.
