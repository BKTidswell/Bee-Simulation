%%CHANGES MADE:
%8/24/2011:
%-- Changed the Skewing method for the Queen's walk to be a normal distribution.


%% THE MODEL:
function [BClump, PRing,hive_hist]=RunModel(Params, hive)

brood=(hive>0).*(hive<22);      %Find brood cells
L=Params.L; W=Params.W;         %Extracts Size of hive
Qp=round([L/2,W/2]);            %Set Queen Position to start in center of hive
hive_hist=zeros(L,W,Params.T);  %Create a storage matrix for daily hive information
BClump=zeros(1,Params.T);  PRing=zeros(1,Params.T);     %Create storage matrices for daily metrics measurements

%% MODEL LOOP- THIS IS THE ACTUAL MODEL IMPLEMENTATION
for d = 1:Params.T
    NectarFLAG=0; %initializes the first part of day as nighttime.
    for hr = 1:24
        if hr>Params.NightHrs %switches time to daytime after Params.NightHrs number of hrs.
            NectarFLAG=1;
        end
        
        % Create random ordering for performing honey/pollen
        % input/output and egg laying. The processes in the hive happen
        % randomly, so this is the best choice for ordering processes.
        LeftToDo=[Params.nEggs,Params.Hi(d)*NectarFLAG,Params.Pi(d)*NectarFLAG, Params.Ho(d),Params.Po(d)];
        while sum(LeftToDo)>0
            p2=LeftToDo/sum(LeftToDo);
            cum_p=cumsum(p2);
            rn      =    rand();
            nexttask  =    find((rn<cum_p),1,'first');
            if nexttask==1       %Queen Lays an Egg
                LayEgg;
                LeftToDo(1)=LeftToDo(1)-1;
            elseif nexttask==2   %A worker Bee brings in a load of Honey
                InputNectar([31,50]);
                LeftToDo(2)=LeftToDo(2)-1;
            elseif nexttask==3   %A worker bee brings in a load of Pollen
                InputNectar([51,70]);
                LeftToDo(3)=LeftToDo(3)-1;
            elseif nexttask==4   %A nurse bee or hungry bee takes a load of honey
                LoadsTaken=OutputNectar([31,50], LeftToDo(4));
                LeftToDo(4)=LeftToDo(4)-LoadsTaken;
            elseif nexttask==5   %A nurse bee or hungry bee takes a load of pollen
                LoadsTaken=OutputNectar([51,70], LeftToDo(5));
                LeftToDo(5)=LeftToDo(5)-LoadsTaken;
            end
        end
        GrowEggs;
        %PlotHive;              %Optional hourly hive plotting
    end
    
%    PlotHive                   %Optional daily hive plotting
    hive_hist(:,:,d)=hive;      %Store daily hive information
    [BClump(1,d),PRing(1,d)]=PatternAnalysis(hive); %store daily metrics measurements
end
%figure; PlotHiveEnd            %Optional end of run plot


%% Queen moves one step and lays one egg if possible.
    function LayEgg
        MoveQueen
        CheckCell


        function MoveQueen
            if Params.QMtype==1
                %Picks a uniform random direction (Qdir) for queen to move next
                Qdir=rand()*2*pi;

            elseif Params.QMtype==2
                %Picks a direction (Qdir) which is skewed to the center of
                %frame. This is more pronounced the further from center she
                %is.
                dir=[L/2,W/2]-Qp; %finds vector pointing to center of hive
                dist=sqrt(sum(dir.^2)); %distance to center of hive
                if dist>0
                    %If not in center angle is from Queens cell to center
                    theta=cart2pol(dir(1),dir(2));
                else
                    %Queen in center so direction completely random.
                    theta=rand()*2*pi;
                end

                %Queen Skewing method: Normal Distribution for Angle
                Qdir=mod(random('Normal',theta,(pi-dist*pi/((Params.L/2)^2+(Params.W/2)^2))),2*pi);
            else
                'ERROR: QUEEN MOVEMENT PARAMETER (Qm) IS NOT 1 OR 2'
            end
            Qp = Qp + [round(1.45*[cos(Qdir),sin(Qdir)])];
            %the 1.45 ensures that she can move into diagonal cells but not more than 1 cell away in any direction
            Qp = min(max(Qp,[1,1]),[Params.L,Params.W]); %IF QUEEN FALLS OFF FRAME, CRUDELY PUT HER BACK ON
        end

        function CheckCell
            %Identifies acceptable cells for new brood, update hive and brood:
            if hive(Qp(1),Qp(2)) == 0   %Cell is empty
                %Checks whether cell is within BCradius of at least
                %BCthreshold brood cells.
                y1=max(Qp(1)-Params.BCradius,1); y2=min(Qp(1)+Params.BCradius,L);
                y3=max(Qp(2)-Params.BCradius,1); y4=min(Qp(2)+Params.BCradius,W);
                EggLaid=(sum(sum(brood(y1:y2,y3:y4)))>=Params.BCthreshold);     %1 is cell is acceptable, 0 otherwise
                hive(Qp(1),Qp(2)) = hive(Qp(1),Qp(2))+EggLaid;   %Leave contents alone if no egg is laid
                brood(Qp(1),Qp(2)) = brood(Qp(1),Qp(2))+EggLaid; %Changes contents to a 1 day old egg if an egg is laid
            end
        end
    end


%% NECTAR INPUT
% The probability of choosing each cell is uniform random from all cells
% in the hive. Cells which are empty or contain the same type of nectar
% recieve a load when chosen. There are 6 tries to get a suitable cell
% before the bee gives up.  This is to try to mimic Camazines decreasing
% amount brought in as comb fills.
    function InputNectar(limits)
        PartialNectar = (hive <= limits(2)-1).*(limits(1) <= hive);
        %Finds partially filled nectar cells of desired type
        Nectar  =    (hive == 0) + PartialNectar;
        %Finds cells which can accept the desired type of nectar.

        temp1=randperm(L*W); choice1 = temp1(1:Params.NumTriesNectarIn);
        if sum(sum(Nectar(choice1)))==0
            %'No Available Cells for Nectar Input'
            return  %End now if there is nowhere to put the nectar.
        else
            choice=choice1(find(Nectar(choice1)==1,1,'first'));
            if hive(choice) == 0
                hive(choice) = limits(1);
                return %quit looking, an available cell was found
            elseif ((hive(choice) >= limits(1)) * (hive(choice) < limits(2))) ==1
                hive(choice) = hive(choice) + 1;
                return %quit looking, an available cell was found
            end
        end
    end


%% NECTAR OUTPUT
% NOtypeP   =1 -> is a uniform random distribution for all cells
%           =2 -> is linearly proportional to # of brood cells within
%                  NOradiusP cells
% NOtypeA   =1 -> One load per choice of cell
%           =2 -> Number of loads linearly dependent on the number of
%                    neighboring brood cells
% Allows 6 attempts to find a suitable cell from randomly chosen cells.

    function LoadsTaken=OutputNectar(limits, LoadsLeft)
        nectar = (limits(1) <= hive) .* (hive <= limits(2));
        PartialNectar = (limits(1) <= hive) .* (hive < limits(2));

        if sum(nectar(:)) == 0  %These is no nectar in the hive to take
            LoadsTaken=1;  %Count the effort to take nectar, even though unsuccessful.
            return
        end


        if Params.NOtypeP==1  %Uniform Random probability of choosing all available cells
            p        =   ones(Params.L,Params.W)/(L*W);
        elseif Params.NOtypeP==2   %cells more likely to be chosen near more brood cells- linear
            A    =   DensityMask((hive>0).*(hive<22),Params.NOradius)/((Params.NOradius*2+1)^2-1);
            %fraction of neighboring cells that contain brood.
            A=1+(A*(Params.AmtNO-1)); %rescales so that the value increases from 1 (no neighbors) to 10 (all brood neighbors)
            if sum(A(:))==0
                p   =   ones(Params.L,Params.W)/(L*W);
            else
                p   =   A/sum(sum(A));
            end
        else
            'EROR: Nm is not a valid input (1,2,3, or 4)'
        end
        cum_p   =    reshape(cumsum(reshape(p,1,Params.L*Params.W)),Params.L,Params.W);
        temp_tries=0;
        while temp_tries<Params.NumTriesNectarOut  %give them Params.NumTriesNectar tries to find a nectar cell befor giving up.
            rn      =    rand();
            choice  =    find((rn<cum_p),1,'first');

            if nectar(choice)==0
                LoadsTaken=1;
                temp_tries=temp_tries+1;
            elseif nectar(choice)==1
                if  Params.NOtypeA==1  %take one nectar trip from each chosen cell.
                    if hive(choice)==limits(1)
                        hive(choice) = 0;
                    else
                        hive(choice) = hive(choice)-1;
                    end
                    LoadsTaken=1;

                elseif Params.NOtypeA==2 %take nectar proportional to # of nearby brood cells
                    if hive(choice)==limits(1)
                        hive(choice) = 0;
                        LoadsTaken=1;
                    else
                        [row,col]=ind2sub(size(hive),choice);
                        neighbors=brood(max(row-Params.NOradius,1):min(row+Params.NOradius,L), max(col-Params.NOradius,1):min(col+Params.NOradius,W));
                        temp=round(sum(sum(neighbors))*((Params.AmtNO-1)/((2*Params.NOradius+1)^2-1))+1);
                        LoadsTaken=min(temp,LoadsLeft);
                        if LoadsTaken<hive(choice)-limits(1)+1
                            hive(choice) = hive(choice)-LoadsTaken;
                        else
                            LoadsTaken=hive(choice)-limits(1)+1;
                            hive(choice) = 0;
                        end
                    end
                else
                    'ERROR: NOtypeA is not a valid input (1 or 2)'
                end
                return; % Nectar was taken so exit loop and stop looking for cells.
            end
        end
    end

%% GROWING THE EGGS BY THE CHOSEN METHOD
    function GrowEggs

        %Ages eggs and hatchs eggs out, depending on the method of egg development chosen.
        % Ed=1 chooses to have all eggs hatch at the end of the day that they are 21.
        % Ed=2 makes each egg older each hour and allows eggs to hatch
        % throughout the day depending on their age
        % Ed=3 ages eggs by day, but randomly hatches a proportion or 21 day
        % olds each hour.
        %hatching throughout the day

        if Params.EDtype==2
            brood= brood- (hive>=21).*(hive<22); %Empties brood cells age >=21
            hive = hive + .042*(hive>0) .* (hive<21) - hive.* (hive>=21).*(hive<22);
            %Adds a fraction of a day to each brood cell's age and
            %empties brood cells >21 days old
        elseif (Params.EDtype==1)|(Params.EDtype==3)
            if Params.EDtype==3
                temp=PickEggs;
                brood=brood-temp;
                hive=hive-hive.*temp;
            end
            if hr==24
                if Params.EDtype==1
                    readybrood=(hive>=21).*(hive<22);
                    brood=brood-readybrood;
                    hive=hive-hive.*readybrood;
                end
                hive = hive + (hive>0) .* (hive<21);
            end
        else
            'ERROR IN CHOICE OF EGG DEVELOPMENT METHOD: Ed MUST BE 1,2, OR 3.'
        end

        function temp = PickEggs
            readybrood=(hive>=21).*(hive<22);
            hive=hive-hive.*readybrood+readybrood.*21.95; %set all ready brood equal to 21.95
            num=round(sum(sum(readybrood))/(25-hr));
            if (num==0)&&(sum(sum(readybrood))/25>0)
                num=(rand(1)<.05);
            end
            temp=hive*0;
            if num>0
                positions=find(readybrood==1);
                k=length(positions);
                if k>0
                    inds=randperm(k);
                    pick=positions(inds(1:num));
                    temp(pick)=1;
                end
            end
        end
    end

%% Make a matrix of the distance to the nearest brood cell (up to radius)
%% All cells distance greater than Radius get a value of Radius
    function mask = DistMask(InputMask, InputRadius)
        [L,W]=size(InputMask);
        mask1 = diag(ones(W,1))+diag(ones(W-1,1),1)+diag(ones(W-1,1),-1);
        mask2 = diag(ones(L,1))+diag(ones(L-1,1),1)+diag(ones(L-1,1),-1);
        b       = mask2*InputMask*mask1; %makes the front 3 cells wide
        mask    = (b.*(InputMask==0))>0; %makes mask=1 if 1 cell away, and 0 if currently occupied
        for i=1:InputRadius-1
            b       = mask2*(b>0)*mask1; %expands by 1 each iteration
            mask    = mask+(i+1)*(((b>0)-InputMask-(mask>0))>0);
        end
        mask = mask + InputRadius*((mask==0)-(InputMask>0));
    end

%% Make a matrix of the number of brood cells within radius of each cell.
    function mask = DensityMask(InputMask,InputRadius)
        [L,W]=size(InputMask);
        mask1   = diag(ones(W,1));
        mask2   = diag(ones(L,1));
        for i=1:InputRadius
            mask1 = mask1 + diag(ones(W-i,1),i) + diag(ones(W-i,1),-i);
            mask2 = mask2 + diag(ones(L-i,1),i) + diag(ones(L-i,1),-i);
        end
        mask = mask2*InputMask*mask1; %makes the front 3 cells wide
    end

%%Analyse pattern formation- We have chosen 2 measures of pattern
%%formation
    function [BClumpt,PRingt]=PatternAnalysis(hive)
        brood=(hive>0).*(hive<22); %Find brood cells
        %Brood Density: avg number of brood neighbors for each brood cell
        n=sum(brood(:));
        v=DensityMask(brood,1).*brood;
        BClumpt=sum(v(:))/n; %avg number of brood neighbors for each brood cell

        %Pollen Location: avg distance from each honey cell to the nearest
        %brood cell
        honeym=(hive>=31).*(hive<=50);
        nh=sum(sum(honeym(:)));
        v=DistMask(brood,35).*honeym;
        PRingt=sum(sum(v(:)))/nh; %quantitative estimate of average distance from brood cell to nearest pollen cell;
    end

%%Plot Hive to track pattern development
    function PlotHive
        h = floor(hive(end:-1:1,:)); h(Params.L+1,Params.W+1)=71;
        [BClumpt,PRingt]=PatternAnalysis(hive);
        pcolor(h); colormap(Params.Color2); title(['Day = ', int2str(d),', Pattern Assessment: clumped:',int2str(BClumpt),',Pollen ring:',int2str(PRingt)]);
        pause(.1);
    end

%%Plot Hive including the Queen's current position- useful for tracking
%each individual action and checking the Queens movement and brood
%placement
    function PlotHiveQp
        h=floor(hive);
        h(Qp(1),Qp(2))=25;
        h = h(end:-1:1,:); h(Params.L+1,Params.W+1)=71;
        pcolor(h); colormap(Params.Color2); %title(['Day = ', int2str(d),', Hour = ',int2str(hr)]);pause(.1);
    end

%%Plot Hive at end of the run, title includes details about the parameters.
    function PlotHiveEnd
        h = floor(hive(end:-1:1,:)); h(Params.L+1,Params.W+1)=71;
        pcolor(h); colormap(Params.Color2); %title(['Pattern Assessment: centered:',int2str(dist),', clumped:',int2str(e1),',ring:',int2str(e2)]);pause(.1);
    end
end

