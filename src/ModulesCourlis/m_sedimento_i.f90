Module M_Sedimento_I

Interface

Subroutine Sedimento  (  &

  QVaseCouche      ,  & ! Flux de dépôt des vases par couche (> 0 dépôt, < 0 érosion)
  QSableCouche    ,  & ! Flux de dépôt des sables par couche (> 0 dépôt, < 0 érosion)
  TauH        ,  & ! Contrainte hydraulique locale (depend du tirant d'eau local)
  TauHMoy        ,  & ! Contrainte hydraulique moyenne dans la section
  TauHMax        ,  & ! Contrainte hydraulique maximale dans la section
  TauE        ,  & ! Contrainte hydraulique effective (depend du rayon hydr.)
  TauEMoy        ,  & ! Contrainte hydraulique effective moyenne ds section
  TauEMax        ,  & ! Contrainte hydraulique effective maximale ds section
  Ceq          ,  & ! Concentration d'equilibre des sables locale
  CeqMoy        ,  & ! Concentration d'equilibre des sables moyenne dans la section
  DeltaH        ,  & ! Variation de hauteur sédimentaire en chaque point des profils
  ProfilCourlis    ,  & ! Profils sedimentaires
    NbProfil      ,  & ! Nombre de profils
  CoucheSed      ,  & ! Paramètres sédimentaires des différentes couches
  NbCouche      ,  & ! Nombre de couches
  CVase        ,  & ! Concentration des vases en suspension
  CSable        ,  & ! Concentration des sables en suspension
    Dt          ,  & ! Pas de temps
  Zsurf        ,  & ! Cote de la surface libre
  Vitesse        ,  & ! Vitesse moyenne par section
    SurfMouil      ,  & ! Surface mouillee
  PerimMouil      ,  & ! Périmètre mouillé
  LimiteDepotG    ,  & ! numéro du point au delà duquel il peut y a voir dépôt ou érosion (1er pt immergé)
  LimiteDepotD    ,  & ! numéro du dernier pt pour lequel il peut y a voir dépôt ou érosion (dernier pt immergé)
  LimiteSable      ,  & ! % de sable à partir duquel la couche est traitée suivant les lois du sable
  CalcSable           ,  & ! choix calcul avec sable
  Erreur        )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       07/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction :  Calcul des termes sources de dépôt-érosion QVaseCouche et
!  --------    QSableCouche (kg/m/s).
!
!
!  * Pour les vases
!    Contrainte sur le fond : TauH = rho.g.h.v^2 / (Kp^2 . Rh^4/3)
!
!    Flux d'érosion (formule de Parthéniades) :
!      QErosion = M.(TauH / TauCE -1) pour TauH > TauCE
!
!    Flux de dépôt (formule de Krone) :
!      QDépôt = Wc.CVase.(1 - TauH / TauCD) pour TauH < TauCD
!
!  * Pour les vases
!    Contrainte sur le fond : TauE = rho.g.v^2 / (Ktot^2 . Rh^1/3)
!
!    Concentration d'équilibre (loi d'Engelund Hansen) :
!      Qv = 0,05.(D50/g/1,6)^1/2 .Ktot^2 .Rh^1/3 / (g.rho) .TauE^5/2
!          / ((rho_s - rho).g.D50)^3/2
!      Ceq = rho_s . Qv . largeur du lit / Q
!
!    Flux d'érosion :
!      QErosion = Wc .(Ceq - CSable) pour CSable < Ceq
!
!    Flux de dépôt :
!      QDépôt = Wc .(CSable - Ceq) pour CSable > Ceq
!
!
!  Sous-programme appelant : DansLo
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!=========================================================================

use M_PRECISION        ! Definition de la precision DOUBLE ou SIMPLE
use M_PARAMETRE_C      ! Definition des constante tq EPS*, W0, ...
use M_PROFIL_COURLIS_T    ! Definition du type PROFIL_COURLIS
use M_COUCHE_T        ! Définition du type COUCHE_T

use M_ERREUR_T        ! Type ERREUR_T
use M_MESSAGE_C        ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'errreur

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  real(DOUBLE)          :: Dt
  integer, intent(in)   :: NbProfil
  integer, intent(in)   :: NbCouche
  logical :: CalcSable

  type(COUCHE_T),         dimension(:)  , intent(in   )  :: CoucheSed
  real(DOUBLE),           dimension(:)  , intent(in   )  :: CVase, CSable

  real(DOUBLE),           dimension(:)  , intent(in   )  :: Zsurf, Vitesse, SurfMouil, PerimMouil
  real(DOUBLE)              , intent(in   )  :: LimiteSable
  integer,                dimension(:)  , intent(in   )  :: LimiteDepotG, LimiteDepotD

  type(PROFIL_COURLIS_T), dimension(:)  , intent(inout)  :: ProfilCourlis

! Variables de sortie
  real(DOUBLE),           dimension(:,:), intent(inout) :: QVaseCouche, QSableCouche
  real(DOUBLE),           dimension(:,:), intent(  out) :: TauH, TauE, Ceq
  real(DOUBLE),           dimension(:)  , intent(  out) :: TauHMoy, TauHMax, TauEMoy, TauEMax, CeqMoy
  real(DOUBLE),           dimension(:,:), intent(  out) :: DeltaH

! Variables locales
! PU2017 : Mise en commentaire des var loc
!  integer     :: i, j, k, l
!  integer     :: NPt              ! Nombre de point de la section en travers i
!  real(DOUBLE) :: Rho, RhoS, W52, W14      ! constantes du calcul
!  real(DOUBLE) :: V, Sm, Rh, Rh43, Rh13, Zsl, H  ! variables hydrauliques du profil
!  real(DOUBLE) :: Cv, Cs            ! concentrations en suspension au profil i
!  real(DOUBLE) :: dxj              ! pas d'abscisse angulaire du profil
!  real(DOUBLE) :: K_p, K_t, Tce, M, d_50, Cf, W, Tcd, Ps  ! paramètres sédimentaires de la couche affleurante
!  real(DOUBLE) :: TaH, TauHMa, TauHMo, TaE, TauEMo, TauEMa, Qv, Ce, CeqMo ! variables intermédiaires de calcul
!  real(DOUBLE) :: Flux              ! variables intermédiaires de calcul
!  real(DOUBLE) :: Depo, HDepo, VolDep      ! variables intermédiaires de calcul (flux, volume, hauteur de dépôt)
!  real(DOUBLE) :: Eros, HEros          ! variables intermédiaires de calcul (flux, hauteur d'érosion)
!  real(DOUBLE) :: VolPot            ! Volume érodable potentiel (si la couche n'est pas limitée en épaisseur)
!  real(DOUBLE) :: VolDis            ! Volume de sédiment disponible pour l'érosion
!  real(DOUBLE) :: z_ref              ! point bas des interfaces sedimentaires
!  logical      :: Vaseux            ! a vrai si la couche suit les lois des vases
!  real(DOUBLE), dimension(:), allocatable :: Xj      ! abscisse transversale du profil en cours
!  integer    , dimension(:), allocatable :: Couche    ! numéro de la 1ere couche non vide en chaque point de profil
!  real(DOUBLE), dimension(:), allocatable :: ErosPot  ! flux d'erosion potentiel en chaque pt du profil


! Traitement des erreurs
! PU2017 : Mise en commentaire des var loc pour traitement erreur
  type(ERREUR_T), intent(inout) :: Erreur
!  integer            :: retour  ! Code de retour de la fonction read, allocate
!  character(132) :: arbredappel_old      ! Ancien arbre d'appel

!=========================================================================

End Subroutine Sedimento

End Interface

End Module M_Sedimento_I
