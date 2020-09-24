Module M_COUCHE_T

use M_PRECISION

  TYPE :: COUCHE_T
    sequence
    character(30)               :: Nom    ! Nom de la couche
    real(DOUBLE)                :: Cfond  ! Concentration de la couche
    real(DOUBLE)                :: Psable ! Pourcentage de sable
    real(DOUBLE)                :: D50    ! Diamètre moyen du sable
    real(DOUBLE)                :: Wc     ! Vitesse de chute
    real(DOUBLE)                :: TauCE  ! Contrainte critique d'érosion
    real(DOUBLE)                :: Mpart  ! Coefficient des Parthéniades
    real(DOUBLE)                :: Kp     ! Strickler de peau
    real(DOUBLE)                :: Kt     ! Strickler total
    real(DOUBLE)                :: TauCD  ! Contrainte critique de dépot des vases
                        ! (Utilise uniquement pour la couche 1)
  END TYPE COUCHE_T

End module M_COUCHE_T
