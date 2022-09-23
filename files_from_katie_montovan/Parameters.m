%NOTES:
%% Queen:
% Movement can be chosen to be a random walk with uniform random
%    direction.
%  Or direction can be chosen to be gaussian normally distributed
%    direction with mean direction oriented towards the center and
%    sandard deviation determined by the distance to the center.
%  Or direction chosen to be gaussian normally distributed towards a
%    particular angle from the direction she came from (30, 45, 90
%    degrees...)
% Distance traveled is gaussian distributed with mean=1, and std=1/2
%    (then rounded to choose cell it would land in)
%% Brood:
% Can have restrictions except that the cell must be available,
% Or, can be restricted to cells within a certain distance of other cells,
% Or, can be allowed only in cells with a threshold number of brood
%    cell neighbors.
% Or, restricted to being within a certain number of steps on the
%    Queens random walk from the last laid or observed brood cell.
%% Egg Development:
% Can be chosen to either occur once per day, or hourly. Hourly
% hatching can either be strictly by hourly age, or can be randomly
% chosen from all 21 day-old brood.
%% Nectar Input Method:
% Handlers can choose cells (from all suitable cells) randomly,
% preferentially away from brood cells, preferentially into partially
% filled cells. The Methods for Honey and Pollen can be chosen to be
% different or to have differing applicable radii.
%% Nectar Output Method:
% Nectar can be chosen from randomly chosen cells, preferentially near
% brood, or preferentially from partially filled cells. The amount
% taken per selection can be chosen to be either 1 load, or dependent
% on the number of neighboring brood cells



function [ModelBrood,ModelNectar,Params, hive]=Parameters()
clear all
Model=struct(); Params=struct();

%% Define Model Parameters
% Queen movement: 
%random walk (Qm = 1), or skewed to center of hive (Qm =
% 2), or skewed to some (45, 90,...) degree angles from where she came
% from (Qm = 3)
% nEggs is the number of steps (attempted egg layings) each hour
    ModelBrood.QMtype = 1; ModelBrood.nEggs=60;
    
% Brood Cell Requirements: 
 % No Requirements (BCtype = 0): cells must only be empty for the queen to lay
 %     in them.
 % Density Dependent (BCtype = 1): cells must have at least 'BCthreshold' brood cells 
 %     within a distance 'BCradius'. 
 % Distance Dependent (BCtype =1): cells must be at most distance
 %    'BCradius' from the nearest brood cell. (Set BCthreshold=1)
 % Second type of distance dependence: cells must be within 'radius'
 %     number of steps from the last laid or observed by brood cell. 
 %     brood cell on random walk: (BCtype = 2)
    ModelBrood.BCtype = 1;    ModelBrood.BCradius = 4;  ModelBrood.BCthreshold=1; 
    
% Egg development: all eggs hatch at the end of each day (Ed = 1), every 
 % hour with age tracked hourly (Ed = 2), or every hour a random subset of
 % 21 day old brood hatch (Ed = 3). 
    ModelBrood.EDtype = 3;
    
% Nectar Input method: Handlers choose cells for depositing nectar
 % according to the probability distributions described below.
    % Uniform random into empty or partially filled cells ([H/P]Itype = 1)
    % Preferential away (at lease 'HIradius'/'PIradius') from brood cells ([H/P]Itype = 2)
    % Preferential into partially filled cells (partial cells draw bees
    %      that randomly choose a cell within Model.HIradius of them) 
    %      ---THIS PIECE OF THE PROGRAM COULD BE REFINED- IT IS A BIT ROUGH, 
    %      BUT DOES THE JOB. ([H/P]Itype = 3)
  % NOTE: different methods can be chosen for honey (H) and pollen (P)
    ModelNectar.HItype = 1; ModelNectar.HIradius=4;
    ModelNectar.PItype = ModelNectar.HItype; 
    ModelNectar.PIradius=ModelNectar.HIradius;
    
% Nectar Output method: nectar is taken from appropriate cells that are
 % chosen from the following distributions, with the specified amounts
 % taken in each trip.
    %Options for probability of choosing particular cells:
        %Uniform random between all cells containing desired product
        %      (Model.[H/P]OtypeP = 1)
        %Probability linearly proportional to number of brood cells within
        %      Model.[H/P]OradiusP cells   ([H/P]OtypeP = 2)
        %Preferential from partially filled cells- likelihood higher for
        %      partially full cells near completely full cells.
        %      Neighborhood of influence has radius: Model.[H/P]OradiusP 
        %      (Model.[H/P]OtypeP = 3)
        
    % Options for amount taken each time cell is selected:
        %One trip (Model.HOtypeA = 1)
        %number of loads is linearly dependent on number of neighboring
        %      brood cells (within HOradiusA) (HOa/Poa = 2)

    %HONEY VALUES:
    ModelNectar.HOtypeP=1;  ModelNectar.HOradiusP=4;  
    ModelNectar.HOtypeA=2; ModelNectar.HOradiusA=4;
    %POLLEN VALUES: SAME AS HONEY FOR MODEL CHOICES
    ModelNectar.POtypeP=ModelNectar.HOtypeP;
    ModelNectar.POradiusP=ModelNectar.HOradiusP;  
    ModelNectar.POtypeA=ModelNectar.HOtypeA; 
    ModelNectar.POradiusA=ModelNectar.HOradiusA;
    
%% Define important Parameters:
    Params.L   = 45; % height in number of cells of the hive (45)
    Params.W   = 75; % width in number of cells of the hive (75)
    Params.T   = 90; % simulation length in days (28)
    
    Params.NightHrs=12; %Number of nighttime hours each day.
    
    Rp  = .99;  % ratio of pollen usage to pollen input
    Rh  = .59;  % ratio of honey usage to honey input

    factor = 1; % An adjustment factor for honey and pollen input for smaller frame size
    gPload = 30/1000; % grams per load.

    
    % Calculate daily honey/pollen input/output in terms of loads per day.
    % ADD SEASONAL VARIABILITY LATER
    % (kg nectar/season)* (1000 g/kg) / (daytime hours/season) / (g/load)
    Params.Hi  = round(factor*ones(1,Params.T) * ((60*1000)/(240*(24-Params.NightHrs)))/gPload);  %60 kg honey/season
%    Params.Pi  = round(factor*ones(1,Params.T) * ((20*1000)/(240*(24-Params.NightHrs)))/gPload);  %20 kg pollen/season
    Params.Pi  = round(factor*2*rand(1,Params.T)*((20*1000)/(240*(24-Params.NightHrs)))/gPload);  %20 kg pollen/season available in uniform random amounts throughout the season, 

    % Calculate Hourly Honey/Pollen usage in loads:
    Params.Ho  = round((Rh*((60*1000)/(240*24))/gPload)*(factor*ones(1,Params.T)));
    Params.Po  = round((Rp*((20*1000)/(240*24))/gPload)*(factor*ones(1,Params.T))); 
    
% DEFINE COLORS FOR DISPLAY. BROOD=BLUE (1-21), HONEY=ORANGE-BROWN (31-50), 
%   POLLEN=YELLOW-GREEN (51-70) (WITH DARKER COLORS BEING OLDER/FULLER CELLS)
    Params.Color2 =    [[1,1,1]; [[.5:-.5/20:0]',[.9:-.5/20:.4]',.9*ones(21,1)];
                zeros(9,3); [[.9:-.5/19:.4]',[.6:-.6/19:0]',.1*ones(20,1)];
                [[1:-1/19:0]',ones(20,1),.1*ones(20,1)];[1,0,0]];
            
% Initialize the hive with a square of brood in the center, and every other cell empty            
    hive = zeros(Params.L,Params.W);
    hive(round(Params.L/2)-2:round(Params.L/2)+2,round(Params.W/2)-2:round(Params.W/2)+2) = ones(5);
