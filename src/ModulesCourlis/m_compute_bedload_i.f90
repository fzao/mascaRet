Module M_compute_bedload_I

Interface

Subroutine compute_bedload  (  &

  CVase      ,  & ! Concentration des vases en suspension
  CSable      ,  & ! Concentration des sables en suspension
  QVaseCouche    ,  & ! Flux de dï¿½pï¿½t des vases par couche (> 0 dï¿½pï¿½t, < 0 ï¿½rosion)
  QSableCouche  ,  & ! Flux de dï¿½pï¿½t des sables par couche (> 0 dï¿½pï¿½t, < 0 ï¿½rosion)
  QVase      ,  & ! Flux de dï¿½pï¿½t des vases (> 0 dï¿½pï¿½t, < 0 ï¿½rosion)
  QSable      ,  & ! Flux de dï¿½pï¿½t des sables (> 0 dï¿½pï¿½t, < 0 ï¿½rosion)
  QApportVase    ,  & ! Flux de d'apport lineaires des vases (Qapp*Capp) en (kg/s/m)
  QApportSable  ,  & ! Flux de d'apport lineaires des sables (Qapp*Capp) en (kg/s/m)
  TauH      ,  & ! Contrainte hydraulique locale (depend du tirant d'eau local)
  TauHMoy      ,  & ! Contrainte hydraulique moyenne dans la section
  TauHMax      ,  & ! Contrainte hydraulique maximale dans la section
  TauE      ,  & ! Contrainte hydraulique effective locale (depend du rayon hydr.)
  TauEMoy      ,  & ! Contrainte hydraulique effective moyenne ds section
  TauEMax      ,  & ! Contrainte hydraulique maximale dans la section
  Ceq        ,  & ! Concentration d'equilibre des sables locale
  CeqMoy      ,  & ! Concentration d'equilibre des sables moyenne dans la section
!  DeltaH      ,  & ! Variation de hauteur sï¿½dimentaire en chaque point des profils
  ProfilCourlis  ,  & ! Profils sedimentaires
  CL_Vase      ,  & ! CL amont de la concentration en Vase
  CL_Sable    ,  & ! CL amont de la concentration en Sable
  ApportVase    ,  & ! Apports en vase
  ApportSable    ,  & ! Apports en sable
  Apport      ,  & ! Apports hydrauliques
  LoiHydrau    ,  & ! Lois hydrauliques
  LoiConc      ,  & ! Lois de concentration
  TempsCourlis      ,  & ! Temps du calcul
  TempsInitial      ,  & ! Premier temps
    DtCourlis        ,  & ! Pas de temps
  Zsurf1      ,  & ! Cote de la surface libre a t+dt
    Sm0        ,  & ! Surface mouillee a t
    Sm1        ,  & ! Surface mouillee a t+dt
  Vit1      ,  & ! Vitesse moyenne par section a t+dt
  Pm1        ,  & ! Perimetre mouille
  CnuxV      ,  & ! Coefficient de diffusion Vases
    CnuxS      ,  & ! Coefficient de diffusion Sables
  ConsConv        ,  & ! parametres schema convection
  CoucheSed    ,  & ! Paramï¿½tres sï¿½dimentaires des diffï¿½rentes couches
  LimiteDepotG  ,  & ! numï¿½ro du point au delï¿½ duquel il peut y a voir dï¿½pï¿½t ou ï¿½rosion (1er pt immergï¿½)
  LimiteDepotD  ,  & ! numï¿½ro du dernier pt pour lequel il peut y a voir dï¿½pï¿½t ou ï¿½rosion (dernier pt immergï¿½)
  LimiteSable    ,  & ! % de sable ï¿½ partir duquel la couche est traitï¿½e suivant les lois du sable
  CalcSable       ,  & ! Option calcul avec Sable
  Erreur      )


!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER M. Jodeau
!
!  VERSION : 5.1       08-2009    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :  Calcule les termes sources  et resout l'equation de
!  --------    transport (convection - diffusion) des sediments dans l'eau
!
!  Sous-programme appelant : Courlis
!  -----------------------
!
!  Sous-programme appele :  CalcApport
!  ---------------------  Convec
!              Sedimento
!              Diffu
!
!=========================================================================

use M_PRECISION        ! Definition de la precision DOUBLE ou SIMPLE
use M_PARAMETRE_C      ! Definition des constante tq EPS*, W0, ...

use M_PROFIL_COURLIS_T    ! Definition du type PROFIL_COURLIS
use M_COUCHE_T        ! Dï¿½finition du type COUCHE_T
use M_APPORT_T        ! Definition du type APPORT_T
use M_SOURCE_TRACER_T    ! Donnï¿½es des sources d'un traceur
use M_CL_COURLIS_T      ! Definition du type CL_COURLIS_T
use M_LOI_T          ! Definition du type LOI_T
use M_LOI_CONC_T      ! Definition du type LOI_T
use M_CONSTANTES_TRACER_T ! parametres schema convection

use M_ERREUR_T        ! Type ERREUR_T
use M_MESSAGE_C        ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'errreur

use M_CalcApport_I
use M_Convec_I
use M_Sedimento_I
use M_Diffu_I


!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  real(DOUBLE)         :: TempsCourlis, DtCourlis, TempsInitial
  real(DOUBLE), intent(in) :: CnuxV
  real(DOUBLE), intent(in) :: CnuxS
  type(CONSTANTES_TRACER_T) , intent(inout)         :: ConsConv
  logical                  :: CalcSable

  real(DOUBLE)      , dimension(:)  , intent(in   ) :: Zsurf1, Sm0, Sm1, Pm1
  real(DOUBLE)              , intent(in   )  :: LimiteSable
  integer        , dimension(:)  , intent(in   )  :: LimiteDepotG, LimiteDepotD
  type(COUCHE_T)    , dimension(:)  , intent(in   )  :: CoucheSed
  type(LOI_T)      , dimension(:)  , intent(in   )  :: LoiHydrau
  type(LOI_CONC_T)    , dimension(:)  , intent(in   )  :: LoiConc

! Variables de sortie
  type(PROFIL_COURLIS_T), dimension(:)  , intent(inout)  :: ProfilCourlis
  type(CL_COURLIS_T)          , intent(inout)  :: CL_Vase, CL_Sable
  type(SOURCE_TRACER_T), dimension(:)  , intent(inout)  :: ApportVase, ApportSable
  type(APPORT_T)    , dimension(:)  , intent(inout)  :: Apport
  real(DOUBLE)      , dimension(:)  , intent(inout)  :: CVase, CSable
  real(DOUBLE)      , dimension(:,:), intent(inout) :: QVaseCouche, QSableCouche
  real(DOUBLE)      , dimension(:)  , intent(  out) :: QVase, QSable
  real(DOUBLE)      , dimension(:)  , intent(  out) :: QApportVase, QApportSable
  real(DOUBLE)      , dimension(:,:), intent(  out) :: TauH, TauE, Ceq
  real(DOUBLE)      , dimension(:)  , intent(  out) :: TauHMoy, TauHMax, TauEMoy, TauEMax, CeqMoy
!real(DOUBLE)      , dimension(:,:), intent(inout) :: DeltaH
  real(DOUBLE)      , dimension(:)  , intent(inout) :: Vit1


! Variables locales
! PU2017 : Mise en commentaire des var loc
!  integer :: i, k, iapp    ! Compteurs
!  integer :: NbProfil    ! Nombre de profils
!  integer :: NbCouche    ! Nombre de couches sï¿½dimentaires
!  integer :: NbApport    ! Nombre d'apports

!  real(DOUBLE), dimension(:), allocatable :: TV2, TS2    ! Grandeurs convectees (Sm*C) a t+Dt


! Traitement des erreurs
! PU2017 : Mise en commentaire des var loc pour traitement erreur
  type(ERREUR_T), intent(inout) :: Erreur
!  integer            :: retour  ! Code de retour de la fonction read, allocate
!  character(132) :: arbredappel_old      ! Ancien arbre d'appel

!=========================================================================

End Subroutine compute_bedload

End Interface

End Module M_compute_bedload_I
