###############################################################################
#
#    An empirical comparison of (un)correlated random parameter logit and 
#    hybrid choice models for environmental valuation: which model to use?
#
###############################################################################

# Estimating a hybrid choice model for Northern Ireland

# An html output file will be created at the end, which will be Table 8 + Table A3

from biogeme import *
from headers import *
from loglikelihood import *
from statistics import *         

# Demeaning and defining attitudinal questions
Zenv1  = DefineVariable('Zenv1', 0 )
Zenv1  = ( Zenv1 - 3 * (env1 == 1))
Zenv1  = ( Zenv1 - 1 * (env1 == 2))
Zenv1  = ( Zenv1 + 1 * (env1 == 3))
Zenv1  = ( Zenv1 + 3 * (env1 == 4))

Zenv2 = DefineVariable('Zenv2', 0 )
Zenv2 = ( Zenv2 - 3 * ( env2 == 1 ))
Zenv2 = ( Zenv2 - 1 * ( env2 == 2 ))
Zenv2 = ( Zenv2 + 1 * ( env2 == 3 ))
Zenv2 = ( Zenv2 + 3 * ( env2 == 4 )) 

Zenv3 = DefineVariable('Zenv3', 0 )        
Zenv3 = ( Zenv3 - 3 * ( env3 == 1 )) 
Zenv3 = ( Zenv3 - 1 * ( env3 == 2 )) 
Zenv3 = ( Zenv3 + 1 * ( env3 == 3 )) 
Zenv3 = ( Zenv3 + 3 * ( env3 == 4 )) 

Zenv4 = DefineVariable('Zenv4', 0 )        
Zenv4 = ( Zenv4 - 3 * ( env4 == 1 )) 
Zenv4 = ( Zenv4 - 1 * ( env4 == 2 )) 
Zenv4 = ( Zenv4 + 1 * ( env4 == 3 )) 
Zenv4 = ( Zenv4 + 3 * ( env4 == 4 )) 

Zenv5 = DefineVariable('Zenv5', 0 )    
Zenv5 = ( Zenv5 - 3 * ( env5 == 1 ))  
Zenv5 = ( Zenv5 - 1 * ( env5 == 2 ))  
Zenv5 = ( Zenv5 + 1 * ( env5 == 3 ))  
Zenv5 = ( Zenv5 + 3 * ( env5 == 4 )) 

Zenv6 = DefineVariable('Zenv6', 0 )   
Zenv6 = ( Zenv6 - 3 * ( env6 == 1 )) 
Zenv6 = ( Zenv6 - 1 * ( env6 == 2 )) 
Zenv6 = ( Zenv6 + 1 * ( env6 == 3 )) 
Zenv6 = ( Zenv6 + 3 * ( env6 == 4 ))  

Zenv7 = DefineVariable('Zenv7', 0 )   
Zenv7 = ( Zenv7 - 3 * ( env7 == 1 )) 
Zenv7 = ( Zenv7 - 1 * ( env7 == 2 )) 
Zenv7 = ( Zenv7 + 1 * ( env7 == 3 )) 
Zenv7 = ( Zenv7 + 3 * ( env7 == 4 ))      


#  Variable description
 
#  marital_status:  1   single
#                   2   married
#                   3   separated
#                   4   widowed
#                   5   cohabiting 
#         
#  num_children:  number of children living
#
#  education: 1   no education
#             2   primary
#             3   GCSE
#             4   A levels         
#             5   Bachelor
#             6   master
#             7   PhD
#             8   Foundation degree
#          
#  economic_status:  1   employed full time
#                    2   employed part time
#                    3   self-employed
#                    4   retired
#                    5   unemployed
#                    6   homemaker
#                    7   student
#
#  buy_green_energy: 1   yes
#                    2   no
#                    3   don't know
#
#  ideo: political orientation
#        1 (left) - 10 (right)
#
#  income:  1   < 15.000
#           2   15.000-23.500
#           3   23.501-33.800
#           4   33.801-48.000
#           5   48.001-87.500
#           6   < 87.501
#

# Creating variables for estimation

cohabit =  DefineVariable('cohabit', 0 )        
cohabit = ( cohabit + 1 * (  marital_status == 2 ) + 1 * (  marital_status == 5 )) 

numchild =  DefineVariable('numchild', 0 )        
numchild =  num_children 

higheduc =  DefineVariable('higheduc', 0 )        
higheduc = ( higheduc + 1 * (  education > 4 ) ) 

employed =  DefineVariable('employed', 0 )        
employed = ( employed + 1 * (  economic_status < 4 ) )

green =  DefineVariable('green', 0 )        
green = ( green + 1 * (  buy_green_energy == 1 ) ) 

polorient =  DefineVariable('polorient', 0 )        
polorient =  ideo

highincome =  DefineVariable('highincome', 0 )        
highincome = ( highincome + 1 * (  income > 4 ) )
 

 
# Defining the structural equation for latent variable model (starting values from a previous estimation):                                                      
    
bsc_age        = Beta('bsc_age       ',0.00475612,-10000,10000,0,'bsc_age       ' )
bsc_female     = Beta('bsc_female    ',-0.0877267,-10000,10000,0,'bsc_female    ' )
bsc_cohabit    = Beta('bsc_cohabit   ',0.0749705,-10000,10000,0,'bsc_cohabit   ' )
bsc_numchild   = Beta('bsc_numchild  ',-0.103048,-10000,10000,0,'bsc_numchild  ' )
bsc_higheduc   = Beta('bsc_higheduc  ',-0.259338,-10000,10000,0,'bsc_higheduc  ' )
bsc_employed   = Beta('bsc_employed  ',-0.046342,-10000,10000,0,'bsc_employed  ' )
bsc_green      = Beta('bsc_green     ',-0.682125,-10000,10000,0,'bsc_green     ' )
bsc_polorient  = Beta('bsc_polorient ',-0.0280028,-10000,10000,0,'bsc_polorient ' )
bsc_highincome = Beta('bsc_highincome',0.31693,-10000,10000,0,'bsc_highincome' )

omegaLVEnv   = bioDraws('omegaLVEnv') 

LVEnv = (        bsc_age        *  age        
              +  bsc_female     *  female     
              +  bsc_cohabit    *  cohabit    
              +  bsc_numchild   *  numchild   
              +  bsc_higheduc   *  higheduc   
              +  bsc_employed   *  employed   
              +  bsc_green      *  green      
              +  bsc_polorient  *  polorient  
              +  bsc_highincome *  highincome 
              +  omegaLVEnv 
           )
 
#############################
#  Attitudinal Statements
#############################

# Defining variables for the measurement equations (starting values based on previous estimations)

# VARIABLE 1LVEnv
tau1LVEnv1 = Beta('tau1LVEnv1',-0.968202,-10000,10000,0,'tau1LVEnv1' )
delta1LVEnv2 = Beta('delta1LVEnv2',4.37139,0,10000,0,'delta1LVEnv2' )
delta1LVEnv3 = Beta('delta1LVEnv3',2.86571,0,10000,0,'delta1LVEnv3' )
alpha1LVEnv = Beta('alpha1LVEnv',2.00653,-100,100,0,'alpha1LVEnv' )

tau1LVEnv2 = tau1LVEnv1 + delta1LVEnv2
tau1LVEnv3 = tau1LVEnv2 + delta1LVEnv3


# Measurement equation 1LVEnv

me1LVEnv = {
  -3 :     exp(tau1LVEnv1 - alpha1LVEnv * LVEnv)/(1 + exp(tau1LVEnv1  -  alpha1LVEnv * LVEnv)),
  -1 :     exp(tau1LVEnv2 - alpha1LVEnv * LVEnv)/(1 + exp(tau1LVEnv2  -  alpha1LVEnv * LVEnv)) - exp(tau1LVEnv1  -  alpha1LVEnv * LVEnv)/(1+exp(tau1LVEnv1  -  alpha1LVEnv * LVEnv)),
   1 :     exp(tau1LVEnv3 - alpha1LVEnv * LVEnv)/(1 + exp(tau1LVEnv3  -  alpha1LVEnv * LVEnv)) - exp(tau1LVEnv2  -  alpha1LVEnv * LVEnv)/(1+exp(tau1LVEnv2  -  alpha1LVEnv * LVEnv)),
   3 : 1 - exp(tau1LVEnv3 - alpha1LVEnv * LVEnv)/(1 + exp(tau1LVEnv3  -  alpha1LVEnv * LVEnv))
                      } 
################################                     

# VARIABLE 2LVEnv
tau2LVEnv1 = Beta('tau2LVEnv1',-2.98886,-10000,10000,0,'tau2LVEnv1' )
delta2LVEnv2 = Beta('delta2LVEnv2',4.27423,0,10000,0,'delta2LVEnv2' )
delta2LVEnv3 = Beta('delta2LVEnv3',3.73755,0,10000,0,'delta2LVEnv3' )
alpha2LVEnv = Beta('alpha2LVEnv',2.67156,-100,100,0,'alpha2LVEnv' )

tau2LVEnv2 = tau2LVEnv1 + delta2LVEnv2
tau2LVEnv3 = tau2LVEnv2 + delta2LVEnv3


# Measurement equation 2LVEnv

me2LVEnv = {
  -3 :     exp(tau2LVEnv1 - alpha2LVEnv * LVEnv)/(1 + exp(tau2LVEnv1  -  alpha2LVEnv * LVEnv)),
  -1 :     exp(tau2LVEnv2 - alpha2LVEnv * LVEnv)/(1 + exp(tau2LVEnv2  -  alpha2LVEnv * LVEnv)) - exp(tau2LVEnv1  -  alpha2LVEnv * LVEnv)/(1+exp(tau2LVEnv1  -  alpha2LVEnv * LVEnv)),
   1 :     exp(tau2LVEnv3 - alpha2LVEnv * LVEnv)/(1 + exp(tau2LVEnv3  -  alpha2LVEnv * LVEnv)) - exp(tau2LVEnv2  -  alpha2LVEnv * LVEnv)/(1+exp(tau2LVEnv2  -  alpha2LVEnv * LVEnv)),
   3 : 1 - exp(tau2LVEnv3 - alpha2LVEnv * LVEnv)/(1 + exp(tau2LVEnv3  -  alpha2LVEnv * LVEnv))
                      } 

################################                     
                      
# VARIABLE 3LVEnv
tau3LVEnv1 = Beta('tau3LVEnv1',-1.01283,-10000,10000,0,'tau3LVEnv1' )
delta3LVEnv2 = Beta('delta3LVEnv2',2.80273,0,10000,0,'delta3LVEnv2' )
delta3LVEnv3 = Beta('delta3LVEnv3',2.40937,0,10000,0,'delta3LVEnv3' )
alpha3LVEnv = Beta('alpha3LVEnv',1.13045,-100,100,0,'alpha3LVEnv' )

tau3LVEnv2 = tau3LVEnv1 + delta3LVEnv2
tau3LVEnv3 = tau3LVEnv2 + delta3LVEnv3


# Measurement equation 3LVEnv

me3LVEnv = {
  -3 :     exp(tau3LVEnv1 - alpha3LVEnv * LVEnv)/(1 + exp(tau3LVEnv1  -  alpha3LVEnv * LVEnv)),
  -1 :     exp(tau3LVEnv2 - alpha3LVEnv * LVEnv)/(1 + exp(tau3LVEnv2  -  alpha3LVEnv * LVEnv)) - exp(tau3LVEnv1  -  alpha3LVEnv * LVEnv)/(1+exp(tau3LVEnv1  -  alpha3LVEnv * LVEnv)),
   1 :     exp(tau3LVEnv3 - alpha3LVEnv * LVEnv)/(1 + exp(tau3LVEnv3  -  alpha3LVEnv * LVEnv)) - exp(tau3LVEnv2  -  alpha3LVEnv * LVEnv)/(1+exp(tau3LVEnv2  -  alpha3LVEnv * LVEnv)),
   3 : 1 - exp(tau3LVEnv3 - alpha3LVEnv * LVEnv)/(1 + exp(tau3LVEnv3  -  alpha3LVEnv * LVEnv))
                      } 

################################                     
                      
# VARIABLE 4LVEnv
tau4LVEnv1 = Beta('tau4LVEnv1',-1.63037,-10000,10000,0,'tau4LVEnv1' )
delta4LVEnv2 = Beta('delta4LVEnv2',2.64661,0,10000,0,'delta4LVEnv2' )
delta4LVEnv3 = Beta('delta4LVEnv3',2.2439,0,10000,0,'delta4LVEnv3' )
alpha4LVEnv = Beta('alpha4LVEnv',0.962059,-100,100,0,'alpha4LVEnv' )

tau4LVEnv2 = tau4LVEnv1 + delta4LVEnv2
tau4LVEnv3 = tau4LVEnv2 + delta4LVEnv3


# Measurement equation 4LVEnv
 
me4LVEnv = {
  -3 :     exp(tau4LVEnv1 - alpha4LVEnv * LVEnv)/(1 + exp(tau4LVEnv1  -  alpha4LVEnv * LVEnv)),
  -1 :     exp(tau4LVEnv2 - alpha4LVEnv * LVEnv)/(1 + exp(tau4LVEnv2  -  alpha4LVEnv * LVEnv)) - exp(tau4LVEnv1  -  alpha4LVEnv * LVEnv)/(1+exp(tau4LVEnv1  -  alpha4LVEnv * LVEnv)),
   1 :     exp(tau4LVEnv3 - alpha4LVEnv * LVEnv)/(1 + exp(tau4LVEnv3  -  alpha4LVEnv * LVEnv)) - exp(tau4LVEnv2  -  alpha4LVEnv * LVEnv)/(1+exp(tau4LVEnv2  -  alpha4LVEnv * LVEnv)),
   3 : 1 - exp(tau4LVEnv3 - alpha4LVEnv * LVEnv)/(1 + exp(tau4LVEnv3  -  alpha4LVEnv * LVEnv))
                      }  

################################                     
                      
# VARIABLE 5LVEnv
tau5LVEnv1 = Beta('tau5LVEnv1',-2.58586,-10000,10000,0,'tau5LVEnv1' )
delta5LVEnv2 = Beta('delta5LVEnv2',3.78442,0,10000,0,'delta5LVEnv2' )
delta5LVEnv3 = Beta('delta5LVEnv3',3.01041,0,10000,0,'delta5LVEnv3' )
alpha5LVEnv = Beta('alpha5LVEnv',1.67362,-100,100,0,'alpha5LVEnv' )

tau5LVEnv2 = tau5LVEnv1 + delta5LVEnv2
tau5LVEnv3 = tau5LVEnv2 + delta5LVEnv3


# Measurement equation 5LVEnv

me5LVEnv = {
  -3 :     exp(tau5LVEnv1 - alpha5LVEnv * LVEnv)/(1 + exp(tau5LVEnv1  -  alpha5LVEnv * LVEnv)),
  -1 :     exp(tau5LVEnv2 - alpha5LVEnv * LVEnv)/(1 + exp(tau5LVEnv2  -  alpha5LVEnv * LVEnv)) - exp(tau5LVEnv1  -  alpha5LVEnv * LVEnv)/(1+exp(tau5LVEnv1  -  alpha5LVEnv * LVEnv)),
   1 :     exp(tau5LVEnv3 - alpha5LVEnv * LVEnv)/(1 + exp(tau5LVEnv3  -  alpha5LVEnv * LVEnv)) - exp(tau5LVEnv2  -  alpha5LVEnv * LVEnv)/(1+exp(tau5LVEnv2  -  alpha5LVEnv * LVEnv)),
   3 : 1 - exp(tau5LVEnv3 - alpha5LVEnv * LVEnv)/(1 + exp(tau5LVEnv3  -  alpha5LVEnv * LVEnv))
                      }                                                                                                   

################################                     
 
# VARIABLE 6LVEnv
tau6LVEnv1 = Beta('tau6LVEnv1',-0.676797,-10000,10000,0,'tau6LVEnv1' )
delta6LVEnv2 = Beta('delta6LVEnv2',3.5974,0,10000,0,'delta6LVEnv2' )
delta6LVEnv3 = Beta('delta6LVEnv3',1.76423,0,10000,0,'delta6LVEnv3' )
alpha6LVEnv = Beta('alpha6LVEnv',1.38141,-100,100,0,'alpha6LVEnv' )

tau6LVEnv2 = tau6LVEnv1 + delta6LVEnv2
tau6LVEnv3 = tau6LVEnv2 + delta6LVEnv3


# Measurement equation 6LVEnv

me6LVEnv = {
  -3 :     exp(tau6LVEnv1 - alpha6LVEnv * LVEnv)/(1 + exp(tau6LVEnv1  -  alpha6LVEnv * LVEnv)),
  -1 :     exp(tau6LVEnv2 - alpha6LVEnv * LVEnv)/(1 + exp(tau6LVEnv2  -  alpha6LVEnv * LVEnv)) - exp(tau6LVEnv1  -  alpha6LVEnv * LVEnv)/(1+exp(tau6LVEnv1  -  alpha6LVEnv * LVEnv)),
   1 :     exp(tau6LVEnv3 - alpha6LVEnv * LVEnv)/(1 + exp(tau6LVEnv3  -  alpha6LVEnv * LVEnv)) - exp(tau6LVEnv2  -  alpha6LVEnv * LVEnv)/(1+exp(tau6LVEnv2  -  alpha6LVEnv * LVEnv)),
   3 : 1 - exp(tau6LVEnv3 - alpha6LVEnv * LVEnv)/(1 + exp(tau6LVEnv3  -  alpha6LVEnv * LVEnv))
                      }   

################################                     
                      
# VARIABLE 7LVEnv
tau7LVEnv1 = Beta('tau7LVEnv1',0.236787,-10000,10000,0,'tau7LVEnv1' )
delta7LVEnv2 = Beta('delta7LVEnv2',2.69624,0,10000,0,'delta7LVEnv2' )
delta7LVEnv3 = Beta('delta7LVEnv3',1.78781,0,10000,0,'delta7LVEnv3' )
alpha7LVEnv = Beta('alpha7LVEnv',0.517254,-100,100,0,'alpha7LVEnv' )

tau7LVEnv2 = tau7LVEnv1 + delta7LVEnv2
tau7LVEnv3 = tau7LVEnv2 + delta7LVEnv3


# Measurement equation 7LVEnvc

me7LVEnv = {
  -3 :     exp(tau7LVEnv1 - alpha7LVEnv * LVEnv)/(1 + exp(tau7LVEnv1  -  alpha7LVEnv * LVEnv)),
  -1 :     exp(tau7LVEnv2 - alpha7LVEnv * LVEnv)/(1 + exp(tau7LVEnv2  -  alpha7LVEnv * LVEnv)) - exp(tau7LVEnv1  -  alpha7LVEnv * LVEnv)/(1+exp(tau7LVEnv1  -  alpha7LVEnv * LVEnv)),
   1 :     exp(tau7LVEnv3 - alpha7LVEnv * LVEnv)/(1 + exp(tau7LVEnv3  -  alpha7LVEnv * LVEnv)) - exp(tau7LVEnv2  -  alpha7LVEnv * LVEnv)/(1+exp(tau7LVEnv2  -  alpha7LVEnv * LVEnv)),
   3 : 1 - exp(tau7LVEnv3 - alpha7LVEnv * LVEnv)/(1 + exp(tau7LVEnv3  -  alpha7LVEnv * LVEnv))
                      }                           
 
##############################         
# CHOICE MODEL
############################## 

# Defining betas and standard deviations (starting values based on previous estimations)
ASC1 = Beta('ASC1',-0.0518506,-100,100,0,'ASC1' )
ASC3 = Beta('ASC3',-2.92919,-100,100,0,'ASC3' )

bbATTR1hh2 = Beta('bbATTR1hh2',0.604053,-100,100,0,'bbATTR1hh2' )
bbATTR1LVEnvhh2     = Beta('bbATTR1LVEnvhh2    ',-0.177877,-100,100,0,'bbATTR1LVEnvhh2    ' )
sdbATTR1hh2 = Beta('sdbATTR1hh2',0.0619468,-100,100,0,'sdbATTR1hh2' )

bbATTR1hh3 = Beta('bbATTR1hh3',0.921552,-100,100,0,'bbATTR1hh3' )
bbATTR1LVEnvhh3     = Beta('bbATTR1LVEnvhh3    ',-0.273301,-100,100,0,'bbATTR1LVEnvhh3    ' )
sdbATTR1hh3 = Beta('sdbATTR1hh3',0.965874,-100,100,0,'sdbATTR1hh3' )

bbATTR2coast2 = Beta('bbATTR2coast2',-0.206877,-100,100,0,'bbATTR2coast2' )
bbATTR2LVEnvcoast2  = Beta('bbATTR2LVEnvcoast2 ',-0.114763,-100,100,0,'bbATTR2LVEnvcoast2 ' )
sdbATTR2coast2 = Beta('sdbATTR2coast2',0.152851,-100,100,0,'sdbATTR2coast2' )

bbATTR2coast3 = Beta('bbATTR2coast3',-0.402105,-100,100,0,'bbATTR2coast3' )
bbATTR2LVEnvcoast3  = Beta('bbATTR2LVEnvcoast3 ',-0.0784891,-100,100,0,'bbATTR2LVEnvcoast3 ' )
sdbATTR2coast3 = Beta('sdbATTR2coast3',-0.719392,-100,100,0,'sdbATTR2coast3' )

bbATTR3cost = Beta('bbATTR3cost',-3.35847,-100,100,0,'bbATTR3cost' )
bbATTR3LVEnvcost    = Beta('bbATTR3LVEnvcost   ',0.386383,-100,100,0,'bbATTR3LVEnvcost   ' )
sdbATTR3cost = Beta('sdbATTR3cost',1.67366,-100,100,0,'sdbATTR3cost' )

bbATTR4perk1 = Beta('bbATTR4perk1',-0.178907,-100,100,0,'bbATTR4perk1' )
bbATTR4LVEnvperk1   = Beta('bbATTR4LVEnvperk1  ',-0.257346,-100,100,0,'bbATTR4LVEnvperk1  ' )
sdbATTR4perk1 = Beta('sdbATTR4perk1',0.356467,-100,100,0,'sdbATTR4perk1' )

bbATTR4perk2 = Beta('bbATTR4perk2',-0.512263,-100,100,0,'bbATTR4perk2' )
bbATTR4LVEnvperk2   = Beta('bbATTR4LVEnvperk2  ',-0.256119,-100,100,0,'bbATTR4LVEnvperk2  ' )
sdbATTR4perk2 = Beta('sdbATTR4perk2',0.624395,-100,100,0,'sdbATTR4perk2' )

# Defining the random variables
R_bbATTR1hh2       =      ( bbATTR1hh2          + bbATTR1LVEnvhh2    * LVEnv  + sdbATTR1hh2    * bioDraws('RND_ATTR1hh2') )  
R_bbATTR1hh3       =      ( bbATTR1hh3          + bbATTR1LVEnvhh3    * LVEnv  + sdbATTR1hh3    * bioDraws('RND_ATTR1hh3') )  
R_bbATTR2coast2    =      ( bbATTR2coast2       + bbATTR2LVEnvcoast2 * LVEnv  + sdbATTR2coast2 * bioDraws('RND_ATTR2coast2') )  
R_bbATTR2coast3    =      ( bbATTR2coast3       + bbATTR2LVEnvcoast3 * LVEnv  + sdbATTR2coast3 * bioDraws('RND_ATTR2coast3') )  
R_bbATTR3cost      =  -exp( bbATTR3cost         + bbATTR3LVEnvcost   * LVEnv  + sdbATTR3cost   * bioDraws('RND_ATTR3cost') )  
R_bbATTR4perk1     =      ( bbATTR4perk1        + bbATTR4LVEnvperk1  * LVEnv  + sdbATTR4perk1  * bioDraws('RND_ATTR4perk1') )  
R_bbATTR4perk2     =      ( bbATTR4perk2        + bbATTR4LVEnvperk2  * LVEnv  + sdbATTR4perk2  * bioDraws('RND_ATTR4perk2') )                                                                      

# Defining the utilities
 
V1 =  (ASC1                              +
       R_bbATTR1hh2    * alt1attr1hh2    +
       R_bbATTR1hh3    * alt1attr1hh3    +
       R_bbATTR2coast2 * alt1attr2coast2 +
       R_bbATTR2coast3 * alt1attr2coast3 +
       R_bbATTR3cost   * alt1attr3cost   +
       R_bbATTR4perk1  * alt1attr4perk1  +
       R_bbATTR4perk2  * alt1attr4perk2  ) 

V2 =  (R_bbATTR1hh2    * alt2attr1hh2    +
       R_bbATTR1hh3    * alt2attr1hh3    +
       R_bbATTR2coast2 * alt2attr2coast2 +
       R_bbATTR2coast3 * alt2attr2coast3 +
       R_bbATTR3cost   * alt2attr3cost   +
       R_bbATTR4perk1  * alt2attr4perk1  +
       R_bbATTR4perk2  * alt2attr4perk2  ) 
         
V3 =  (  ASC3                            +
       R_bbATTR1hh2    * alt3attr1hh2    +
       R_bbATTR1hh3    * alt3attr1hh3    +
       R_bbATTR2coast2 * alt3attr2coast2 +
       R_bbATTR2coast3 * alt3attr2coast3 +
       R_bbATTR3cost   * alt3attr3cost   +
       R_bbATTR4perk1  * alt3attr4perk1  +
       R_bbATTR4perk2  * alt3attr4perk2)          
      
# Associate utility functions with the numbering of alternatives
V = {1: V1,
     2: V2,
     3: V3 }

# Associate the availability conditions with the alternatives
av = {1: one,
      2: one,
      3: one}

# country = 1 -> England
# country = 2 -> NI
# country = 3 -> Scotland

# In python data format, 99999 are missing variables
# Excluding missing variables and outliers
exclude = ( ( country          < 2   ) 
          + ( country          > 2   )
          + ( Double_id        < 2   ) 
          + ( too_short        < 10  )
          + ( age              < 18  )
          + ( age              > 65  )
          + ( Block            > 100 )  
          + ( env1             > 4   )                    
          + ( env2             > 4   )     
          + ( env3             > 4   )          
          + ( env4             > 4   )           
          + ( env5             > 4   ) 
          + ( env6             > 4   ) 
          + ( env7             > 4   ) 
          + ( pay_elecbill     > 6000)      
          + ( marital_status   > 100 ) 
          + ( num_children     > 10  )
          + ( num_adults       > 6   )
          + ( education        > 100 )
          + ( economic_status  > 100 )
          + ( distance_coast   > 900 )
          + ( buy_green_energy > 100 )
          + ( ideo             > 100 )
          + ( income           > 100 ) 
          + ( ChoiceSum        > 29  )) > 0              
          
BIOGEME_OBJECT.EXCLUDE = exclude
          
#         
          
# The choice model is a logit, with availability conditions
prob = bioLogit(V,av,Choice)
          
# Iterator on individuals, that is on groups of rows.
metaIterator('personIter','__dataFile__','panelObsIter','ID' )
          
# For each item of personIter, iterates on the rows of the group. 
rowIterator('panelObsIter','personIter')
          
#Conditional probability for the sequence of choices of an individual
condProbIndiv = Prod(prob,'panelObsIter')
#         
condLikelihoodOneObs = ( condProbIndiv 
                         *Sum(Elem(me1LVEnv ,Zenv1),'panelObsIter')/Sum(1,'panelObsIter') 
                         *Sum(Elem(me2LVEnv ,Zenv2),'panelObsIter')/Sum(1,'panelObsIter') 
                         *Sum(Elem(me3LVEnv ,Zenv3),'panelObsIter')/Sum(1,'panelObsIter') 
                         *Sum(Elem(me4LVEnv ,Zenv4),'panelObsIter')/Sum(1,'panelObsIter') 
                         *Sum(Elem(me5LVEnv ,Zenv5),'panelObsIter')/Sum(1,'panelObsIter') 
                         *Sum(Elem(me6LVEnv ,Zenv6),'panelObsIter')/Sum(1,'panelObsIter') 
                         *Sum(Elem(me7LVEnv ,Zenv7),'panelObsIter')/Sum(1,'panelObsIter')  
                           )
          
# Integration by simulation
probIndiv = MonteCarlo(condLikelihoodOneObs)
          
# Likelihood function
loglikelihood = Sum(log(probIndiv),'personIter')
          
BIOGEME_OBJECT.ESTIMATE = loglikelihood
          
BIOGEME_OBJECT.DRAWS = { 'omegaLVEnv'     : ('NORMAL','ID'),
                         'RND_ATTR1hh2'   : ('NORMAL','ID'),
                         'RND_ATTR1hh3'   : ('NORMAL','ID'),
                         'RND_ATTR2coast2': ('NORMAL','ID'),
                         'RND_ATTR2coast3': ('NORMAL','ID'),
                         'RND_ATTR3cost'  : ('NORMAL','ID'),
                         'RND_ATTR4perk1' : ('NORMAL','ID'),
                         'RND_ATTR4perk2' : ('NORMAL','ID')}

BIOGEME_OBJECT.PARAMETERS['optimizationAlgorithm'] = "CFSQP"
BIOGEME_OBJECT.PARAMETERS['checkDerivatives'] = "0"
BIOGEME_OBJECT.PARAMETERS['numberOfThreads'] = "20"
BIOGEME_OBJECT.PARAMETERS['RandomDistribution'] = "MLHS"
BIOGEME_OBJECT.PARAMETERS['NbrOfDraws'] = "2000"      
BIOGEME_OBJECT.PARAMETERS['Seed'] = "17"    

     