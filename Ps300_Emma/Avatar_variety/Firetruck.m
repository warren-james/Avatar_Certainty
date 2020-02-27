 function Firetruck

Screen('Preference', 'SkipSyncTests', 1);
%% To run the Avatar Experiment
% Setting up the experiment: 
% - Order
%   - There are 3 types of speed distribution 
%   - Each participant will experience all of them
%   - This means there are 6 different orders we can make 
% - Condition
%   - 1: Abstract. Just using squares
%   - 2: Meaning. Uses Firetrucks and buildings 
% - Measure type 
%   - 1: Frequency 
%   - 2: Confidence

%% Need to write something to check resolution 


%% add paths
% to store results
addpath('results/')
% to retrieve images
addpath('images/Abstract_version')
addpath('images/Truck_version')

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Get Participant info %%
%%%%%%%%%%%%%%%%%%%%%%%%%%
% dialogue prompt box 
% experiment details
prompt = {'Enter Participant ID: ','Order (1:6)','Condition (1 or 2)','Measure type (1 or 2)'};
title = 'Input';
num_lines = 1;
defaults = {'99','1','2','2','1'}; % setting condition default to 2 so it uses the firetruck
answer = inputdlg(prompt, title, num_lines, defaults);

% record this
ID = answer{1};
trial.cond_order = str2num(answer{2});    %#ok<ST2NM>
trial.condition_num = str2num(answer{3}); %#ok<ST2NM>
trial.measure = str2num(answer{4});       %#ok<ST2NM>

% demographics 
prompt = {'Age', 'Gender', 'Dominant-Hand', 'Major'};
title = 'Input';
num_lines = 1;
defaults = {'', '', '', ''}; 
answer = inputdlg(prompt, title, num_lines, defaults);

% record this
demo.age = str2num(answer{1});  %#ok<ST2NM>
demo.gender = answer{2};
demo.hand = answer{3};
demo.major = answer{4};

% set filename
filename.demo_phase = strcat('results/', ID, '_demophase.txt');
filename.enddat = strcat('results/', ID, '_enddat.txt');
filename.clickhist = strcat('results/',ID, '_clickhist.txt');

% open files to write into
file.demo_phase = fopen(filename.demo_phase, 'w');
file.enddata = fopen(filename.enddat, 'w');
file.clickhist = fopen(filename.clickhist, 'w');

% specify colnames
fprintf(file.demo_phase, 'Spread,Trial,Delta,Success\n');
fprintf(file.enddata, 'Condition,Spread,Block,Trial,Delta,RT,Initial_x,Placed_x,Target_side,Speed,Success\n');
fprintf(file.clickhist, 'Spread,Block,Trial,Time_elapsed,Delta,click_num,Placed_x\n');

% check measure type and setup appropriate files
if trial.measure == 1
    filename.estimates = strcat('results/', ID, '_estimates.txt');
    file.estimates = fopen(filename.estimates, 'w');
    fprintf(file.estimates, 'Spread,Delta, Estimate\n');
else
    filename.confidence = strcat('results/',ID,'_confidence.txt');
    file.confidence = fopen(filename.confidence, 'w');
    fprintf(file.confidence, 'Spread,Delta,Y_or_N,Confidence\n');
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Get Screen information and set some parameters %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% background Grey
params.grey = 0.4;

% get screen number
screens=Screen('Screens');
if max(screens)==1
    screenNumber=min(screens)+1;
else
    screenNumber=min(screens);
end

% open Screen
[params.stimuliScrn, wRect] = Screen('OpenWindow',0, 255*params.grey);

% set text size and font
Screen('TextSize', params.stimuliScrn, 24);
Screen('TextFont', params.stimuliScrn, 'Helvetica');

% stop keyboard listening in the command window 
ListenChar(2);

% get screen dimensions
[params.x_res, params.y_res] = Screen('WindowSize', params.stimuliScrn);

% sort out centre for drawing things 
[params.centre_x, params.centre_y] = RectCenter(wRect);

% set boundaries for box presentation
params.deltamin = 200;                     % set min separation
params.deltamax = params.centre_x - 100;   % set largest separation

% separations to test
params.step = round(params.deltamax - params.deltamin)/3;
params.delta = params.deltamin:params.step:params.deltamax; % set of deltas

% block and trial info    % defaults:
params.nBlocks = 3;       % 3
params.reps = 1;          % 15
params.repsestimate = 1;  % 10

% for moving object settings 
% get frame duration 
anim.ifi = Screen('GetFlipInterval', params.stimuliScrn);

% box properties 
box.Rect = [0 0 100 50];                                        % Shape of box
box.spray = [0 0 90 30];                                        % Range
box.startx = params.centre_x;                                   % for offsetting
box.starty = params.centre_y;                                   % vertical centre
box.movetime = 100;                                             % travel time
box.delay = round(box.movetime/5) - 1;                          % delay before movement
box.avgspeed = round(median(params.delta)/(box.movetime - 20)); % average speed, might not need
box.maxspeed = ceil(max(params.delta)/(box.movetime)) - 1;      % max speed
box.minspeed = [1 1 1];
box.range = [repelem(box.maxspeed, 3)];
% for the confidence judgements 
box.judgeline = [0 0 params.centre_x 15];
box.judgebox = [0 0 15 45];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Keys and Mouse settings %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% available keys to press 
keys.downKey = KbName('DownArrow');
keys.upKey = KbName('UpArrow');
keys.spaceKey = KbName('space');
keys.y = KbName('y');
keys.n = KbName('n');

% mouse dot colour
mouse.white = [255 255 255];

% hide mouse
HideCursor;

%%%%%%%%%%%%%%%%%%%%%%%%
%% Animation settings %%
%%%%%%%%%%%%%%%%%%%%%%%%
box.speed_beta_options = [1 1; 10 10; 1000 1000];
box.speed_orders = [1 2 3; 1 3 2; 2 1 3; 2 3 1; 3 1 2; 3 2 1];
box.speed_labels = {'uniform' 'broad' 'constant'};

box.order_selected = box.speed_orders(trial.cond_order, 1:3);
box.speed_desc = box.speed_labels(box.order_selected);
% for loop to make list of speed orders to index into 
box.speed_beta = zeros(3,2);
for ii = 1:3
    % get the values we want
    temp_idx = box.order_selected(ii);
    temp_vals = box.speed_beta_options(temp_idx, 1:2);
    box.speed_beta(ii, 1:2) = temp_vals;
end
% tidy
clear temp_idx temp_vals

% need to setup the distributions 
% check condition entered and set box speed distribution
% ditribution settings
% if trial.cond_order == 1
%     variable then constant
%     if trial.spread == 1
%         box.speed_beta = [1 1; 10000 10000]; 
%         box.speed_desc = {'variable' 'constant'};
%         box.range = [box.maxspeed, box.maxspeed];
%         box.minspeed = [1 1];
%     variable then restricted   
%     elseif trial.spread == 2
%         box.speed_beta = [1 1; 1 1]; 
%         box.speed_desc = {'variable' 'restricted'};  
%         box.range = [box.maxspeed, box.maxspeed/2]; 
%         box.minspeed = [1 box.maxspeed/4];
%     constant then restricted
%     elseif trial.spread == 3
%         box.speed_beta = [10000 10000; 1 1]; 
%         box.speed_desc = {'constant' 'restricted'};  
%         box.range = [box.maxspeed, box.maxspeed/2]; 
%         box.minspeed = [1 box.maxspeed/4];
%     end
% elseif trial.cond_order == 2
%     constant then variable
%     if trial.spread == 1
%         box.speed_beta = [10000 10000; 1 1]; 
%         box.speed_desc = {'constant' 'variable'};
%         box.range = [box.maxspeed, box.maxspeed];
%         box.minspeed = [1 1];
%     restricted then variable
%     elseif trial.spread == 2
%         box.speed_beta = [1 1; 1 1]; 
%         box.speed_desc = {'restricted' 'variable'};  
%         box.range = [box.maxspeed/2, box.maxspeed]; 
%         box.minspeed = [box.maxspeed/4 1];
%     restricted then constant
%     elseif trial.spread == 3
%         box.speed_beta = [1 1; 10000 10000]; 
%         box.speed_desc = {'restricted' 'constant'};  
%         box.range = [box.maxspeed, box.maxspeed/2]; 
%         box.minspeed = [box.maxspeed/4 1];
%     end
% end

% sort out indexing part
box.speed_order = [1 2 3]';
box.speed_order = repmat(box.speed_order, 1, params.nBlocks/3)';
box.speed_order = box.speed_order(:)';

% % plot to see what this looks like
% x = 0:0.01:1;
% y = betapdf(x, box.speed_beta(1), box.speed_beta(2));
% plot(x*box.range+box.minspeed,y)

% which avatar is used
if trial.condition_num == 1
    trial.condition = 'square';
    trial.target = 'target';
else
    trial.condition = 'truck';
    trial.target = 'house';
end

% set up the blending part for alpha
Screen('BlendFunction',params.stimuliScrn,'GL_SRC_ALPHA','GL_ONE_MINUS_SRC_ALPHA');

% read in images
if strcmp(trial.condition, 'square')
    % fire truck
    [pict,~,alpha] = imread('images/Abstract_version/Fire_truck.png');
    pict(:,:,4) = alpha;
    box.png.Fire_truck = Screen('MakeTexture', params.stimuliScrn, pict);
    
    % placed truck
    [pict,~,alpha] = imread('images/Abstract_version/Placed_truck.png');
    pict(:,:,4) = alpha;
    box.png.Placed_truck = Screen('MakeTexture', params.stimuliScrn, pict);
    
    % Target buildings
    [pict,~,alpha] = imread('images/Abstract_version/Target_building.png');
    pict(:,:,4) = alpha;
    box.png.Building = Screen('MakeTexture', params.stimuliScrn, pict);
    
    % Burning
    [pict,~,alpha] = imread('images/Abstract_version/Burning.png');
    pict(:,:,4) = alpha;
    box.png.Burning = Screen('MakeTexture', params.stimuliScrn, pict);
    
    % Burnt down
    [pict,~,alpha] = imread('images/Abstract_version/Burnt_down.png');
    pict(:,:,4) = alpha;
    box.png.Burnt_down = Screen('MakeTexture', params.stimuliScrn, pict);

elseif strcmp(trial.condition, 'truck')
    % fire truck front
    [pict,~,alpha] = imread('images/Truck_version/Fire_truck_front.png');
    pict(:,:,4) = alpha;
    box.png.Fire_truck.Front = Screen('MakeTexture', params.stimuliScrn, pict);
    
    % fire truck left
    [pict,~,alpha] = imread('images/Truck_version/Fire_truck_left.png');
    pict(:,:,4) = alpha;
    box.png.Fire_truck.Left = Screen('MakeTexture', params.stimuliScrn, pict);
    
    % fire truck right
    [pict,~,alpha] = imread('images/Truck_version/Fire_truck_right.png');
    pict(:,:,4) = alpha;
    box.png.Fire_truck.Right = Screen('MakeTexture', params.stimuliScrn, pict);
    
    % placed truck 
    [pict,~,alpha] = imread('images/Truck_version/Placed_truck.png');
    pict(:,:,4) = alpha;
    box.png.Placed_truck = Screen('MakeTexture', params.stimuliScrn, pict);
    
    % Target buildings
    [pict,~,alpha] = imread('images/Truck_version/Target_building.png');
    pict(:,:,4) = alpha;
    box.png.Building = Screen('MakeTexture', params.stimuliScrn, pict);

    % Burning
    [pict,~,alpha] = imread('images/Truck_version/Burning.png');
    pict(:,:,4) = alpha;
    box.png.Burning = Screen('MakeTexture', params.stimuliScrn, pict);
    
    % Burnt down
    [pict,~,alpha] = imread('images/Truck_version/Burnt_down.png');
    pict(:,:,4) = alpha;
    box.png.Burnt_down = Screen('MakeTexture', params.stimuliScrn, pict);
end

% Sync to get timestamp
anim.vbl = Screen('flip', params.stimuliScrn);
anim.waitframes = 1;

% max priority, not sure what this does
topPriority = MaxPriority(params.stimuliScrn);
Priority(topPriority);


%%%%%%%%%%%%%%%%%%%%%%
%% Start experiment %%
%%%%%%%%%%%%%%%%%%%%%%

% data frame to store choices
demo_phase = [];
enddata = [];
estimates = [];
clickhist = [];
confidence_data = [];

% for counting blocks
block_count = 0;

% make intro screen
if strcmp(trial.condition, 'square')
    DrawFormattedText(params.stimuliScrn, 'Welcome to the experiment. Your task is to ensure that the red square reaches its target as many times as possible by placing it between two other squares presented on the screen. They are both equally likely to be the target on every trial. \n \n Press space to continue.', 'center', params.centre_y*0.5, [], round(params.x_res/15));
else
    DrawFormattedText(params.stimuliScrn, 'Welcome to the experiment. In this task, there will be buildings, one of which will catch. You are to prevent as many of these buildings as possible from burning down by placing a Firetruck between the two buildings that are both equally likely to catch fire. \n \n \n Press space to continue.', 'center', params.centre_y*0.5, [], round(params.x_res/15));
end
Screen('Flip', params.stimuliScrn, 0);
WaitSecs(1.0);
KbWait;

% run the experiment
for block = 1:params.nBlocks
    block_count = block_count + 1;
    % allow for passing of block info
    trial.block = block;
    
    % set font size and style
    Screen('TextSize', params.stimuliScrn, 24);
    Screen('TextFont', params.stimuliScrn, 'Helvetica');
    
    % sort intro screens
    if block == 1 || block == 2 || block == 3
        % Do the observation phase
        DrawFormattedText(params.stimuliScrn, ['First of all, you will do some practice runs to familiarise yourself with how much distance the ' trial.condition ' will travel. You will see the ' trial.condition ' and one ' trial.target '. At which point, you should press the space bar and the ' trial.condition ' will move towards the ' trial.target '.\n \n Press the spacebar when you are ready to start.'], 'center', params.centre_y*0.5, [], round(params.x_res/15));

        Screen('Flip', params.stimuliScrn, 0);
        WaitSecs(1.0);
        KbWait;
        % give trial info for truck 
        trial.range = box.range(block);
        trial.minspeed = box.minspeed(block);
        
        % observe speed phase
        [demo_phase] = observe_speed(params, box, anim, trial, demo_phase);
        
        % show quick demo
        % only do the demo on for the first block
        if block == 1
            if strcmp(trial.condition, 'square')
                DrawFormattedText(params.stimuliScrn, 'Now you''re familiar with the how the square moves, here''s a demonstration of the task \n Press space to start the demonstration', 'center', params.centre_y*0.5, [], round(params.x_res/15));
            else
                DrawFormattedText(params.stimuliScrn, 'Now you''re familiar with the Truck''s performance, here''s a demonstration of the task \n Press space to start the demonstration', 'center', params.centre_y*0.5, [], round(params.x_res/15));
            end
            Screen('Flip', params.stimuliScrn,0);
            WaitSecs(1.0);
            KbWait;
            Demo_function(params, box, anim, mouse, trial);
        end
        
        % run actual experiment
        DrawFormattedText(params.stimuliScrn, 'The task will now start. \n Press the spacebar to start the block', 'center', params.centre_y-250, [], round(params.x_res/15));
        Screen('flip', params.stimuliScrn, 0);
        WaitSecs(1.0);
        KbWait;
    else
        DrawFormattedText(params.stimuliScrn, ['Block No. ' block_count-1 ' of ' num2str(params.nBlocks) ' completed, feel free to take a break. \n Press the spacebar to start the next block'], 'center', params.centre_y-250, [], round(params.x_res/15));
        Screen('flip', params.stimuliScrn, 0);
        WaitSecs(1.0);
        KbWait;
    end

    % make order of deltas to test
    delta = Shuffle(repmat(params.delta, 1, params.reps));
    
    % reset trial count
    trial.trial = 0;
    
    % run block
    for dist = delta
        % record trial number
        trial.trial = trial.trial + 1;
        % set this to true while they place the truck
        positionPhase = true;
        
        % add to trial struct
        trial.dist = dist;
        
        % get speed for this trial
        [box] = GetSpeed(box, trial);
        
        % place box in a non-central position
        % first decide on offset
        if rand > 0.5
            offset.direction = 1;
        else
            offset.direction = -1;
        end
        
        % how much to offset by
        offset.distance = round(rand(1)*trial.dist/2);
        
        % starting position
        newstart.x = box.startx + offset.distance * offset.direction;
        initialx = newstart.x;
        newstart.y = box.starty + 200;
        
        % run a trial
        [timeelapsed, newstart, build, clickhist] = place_function(box, positionPhase, params, trial, anim, mouse, keys, newstart, clickhist);
        
        % decide which building catches fire
        if rand<0.5
            targ.direction = -1;
        else
            targ.direction = 1;
        end
        
        % set targ location
        targ.location = params.centre_x + targ.direction*trial.dist;
        
        % loop for number of screens defined above
        anim.move = 1;
        anim.carryon = true;
        
        % for range check
        anim.within_range = false;
        
        % success check
        anim.success = 0;
        
        % Animate truck driving to target for that trial
        [enddata] = anim_function(anim, build, targ, box, newstart, params, enddata, initialx, timeelapsed, trial);
    end
 
    % increase block_count
    % block_count = block_count + 1;
    
    if ismember(block, [1:params.nBlocks-1])
        % get starting and finishing index
        start = ((block-1)*params.reps*length(params.delta)) + 1;
        finish = (block*params.reps*length(params.delta));
        % get success rate 
        num_success = (sum(enddata(start:finish,end))/length(enddata(start:finish,end)))*100;

        %num_success = (sum(enddata(:,end))/length(enddata(:,end)))*100;
        
        % now the estimating part is about to begin
        if strcmp(trial.condition, 'truck')
            DrawFormattedText(params.stimuliScrn, ['You are halfway there! In this half, you prevented ' num2str(round(num_success)) '% of the buildings from burning down! There is one last task before you move onto the next half. For this, you will be asked to state whether the ' trial.condition ' would reach the displayed ' trial.target ' and then provide an indication of how certain you are in your choice. Whenever you''re ready, press the spacebar to start the last task.'], 'center', params.centre_y-250, [], round(params.x_res/15));
        else
            DrawFormattedText(params.stimuliScrn, ['You are halfway there! In this half, you reached ' num2str(round(num_success)) '% of the targets in time! \n There is one last task before you move onto the next half. For this, you will be asked to state whether the ' trial.condition ' would reach the displayed ' trial.target ' and then provide an indication of how certain you are in your choice. Whenever you''re ready, press the spacebar to start the last task.'], 'center', params.centre_y-250, [], round(params.x_res/15));
        end
        Screen('Flip', params.stimuliScrn, 0);
        WaitSecs(1);
        KbWait();
        
        % check trial.measure 
        if trial.measure == 1
            % Get participant estimates
            [estimates] = estimate_function(estimates, params, box, anim, keys, trial);
        else
            % Get confidence
            [confidence_data] = confidence_function(confidence_data, params, box, anim, keys, trial, mouse);
        end
        
        % tell participant we're going to a new block of decisions
        if strcmp(trial.condition, 'truck')
            DrawFormattedText(params.stimuliScrn, 'You will now do the same as before, except this time you will be driving a different truck. So, before you start making decisions again, you will have the opportunity to relearn its performance. \n Press the spacebar to continue', 'center', params.centre_y-250, [], round(params.x_res/15));
        else
            DrawFormattedText(params.stimuliScrn, 'You will now do the same as before, except this time the square will behave differently. So, before you start making decisions again, you will have the opportunity to relearn its performance. \n Press the spacebar to continue', 'center', params.centre_y-250, [], round(params.x_res/15));
        end
        
        Screen('Flip', params.stimuliScrn, 0);
        WaitSecs(1);
        KbWait();
        
    elseif block == params.nBlocks
        start = ((block-1)*params.reps*length(params.delta)) + 1;
        finish = (block*params.reps*length(params.delta));
        % get success rate
        num_success = (sum(enddata(start:finish,end))/length(enddata(start:finish,end)))*100;

        % now the estimating part is about to begin
        if strcmp(trial.condition, 'truck')
            DrawFormattedText(params.stimuliScrn, ['One more Task to go! In this half, you saved ' num2str(round(num_success)) '% of buildings from burning down! \n There is one last task before you move onto the next half. In this next section, you will be asked to estimate how often the ' trial.condition ' would reach the taget presented on screen. Whenever you''re ready, press the spacebar to start the last task.'], 'center', params.centre_y-250, [], round(params.x_res/15));
        else
            DrawFormattedText(params.stimuliScrn, ['One more Task to go! In this half, you reached ' num2str(round(num_success)) '% of the targets in time! \n There is one last task before you move onto the next half. In this next section, you will be asked to estimate how often the ' trial.condition ' would reach the taget presented on screen. Whenever you''re ready, press the spacebar to start the last task.'], 'center', params.centre_y-250, [], round(params.x_res/15));
        end
        Screen('Flip', params.stimuliScrn, 0);
        WaitSecs(1);
        KbWait();
        
        % check trial.measure 
        if trial.measure == 1
            % Get participant estimates
            [estimates] = estimate_function(estimates, params, box, anim, keys, trial);
        else
            % Get confidence
            [confidence_data] = confidence_function(confidence_data, params, box, anim, keys, trial, mouse);
        end
    end
end

% get percentage of successful trials 
num_success = (sum(enddata(:,end))/length(enddata(:,end)))*100;

% last screen        
if strcmp(trial.condition, 'truck')
    DrawFormattedText(params.stimuliScrn, ['Thank you for your participation! Overall, you saved ' num2str(round(num_success)) '% of buildings from burning down! \n The experiment will terminate shortly'], 'center', params.centre_y-250, [], round(params.x_res/15));
else
    DrawFormattedText(params.stimuliScrn, ['Thank you for your participation! Overall, you reached ' num2str(round(num_success)) '% of the targets in time! \n The experiment will terminate shortly'], 'center', params.centre_y-250, [], round(params.x_res/15));
end
Screen('Flip',params.stimuliScrn,0);
WaitSecs(3);

% write data files
% demo_phase 
fprintf(file.demo_phase, '%d,%d,%d,%d\n', demo_phase');
fclose(file.demo_phase);

% choice data
fprintf(file.enddata,'%d,%d,%d,%d,%d,%1.3f,%d,%d,%d,%d,%d\n', enddata');
fclose(file.enddata);

% get screen info and parametes used 
% think about what else we might need... 
vals.box.maxspeed = box.maxspeed;
vals.box.speed_beta = box.speed_beta;
vals.box.movetime = box.movetime;
vals.box.spray = box.spray(4);
vals.box.speed_desc = box.speed_desc;
vals.params.centre_x = params.centre_x;
vals.params.centre_y = params.centre_y;
vals.params.delta = params.delta;
vals.params.refresh = anim.ifi; %#ok<STRNU>

% save this
save(strcat('results/', ID, '_vals'), '-struct', 'vals');

% save demographics 
demo.age = str2num(answer{1});  %#ok<ST2NM>
demo.gender = answer{2};
demo.hand = answer{3};
demo.major = answer{4};
save(strcat('results/', ID, '_demo'), '-struct', 'demo');

% clickhist 
fprintf(file.clickhist, '%d,%d,%d,%1.3f,%d,%d,%d\n', clickhist');
fclose(file.clickhist);

% save appropriate measure
if trial.measure == 1
    % estimates
    fprintf(file.estimates,'%d,%d,%d\n',estimates');
    fclose(file.estimates);
else
    % confidence
    fprintf(file.confidence, '%d,%1.3f,%d,%d\n', confidence_data');
    fclose(file.confidence);
end

% close screen
sca

% reset keyboard and mouse
ListenChar();
ShowCursor;

end






%%%%%%%%%%%%%%%%%%%%%%%%
%% Placement function %%
%%%%%%%%%%%%%%%%%%%%%%%%
function [timeelapsed, newstart, build, clickhist] = place_function(box, positionPhase, params, trial, anim, mouse, keys, newstart, clickhist)
% start timer
tic

% put mouse on screen
mouse_x_offset = randi(20)*10;
if rand < 0.5 
    mouse_x_offset = mouse_x_offset * -1;
end

SetMouse(params.centre_x + mouse_x_offset, params.centre_y - 200, params.stimuliScrn);

% reset num_clicks
num_clicks = 0;
mousedown = false;

% setup some building objects 
Left_building = box.png.Building;
Right_building = box.png.Building;

% setup firetruck 
if strcmp(trial.condition, 'square')
    firetruck = box.png.Fire_truck;
else
    firetruck = box.png.Fire_truck.Front;
end

% list of buildings 
to_draw.objects = [Left_building Right_building firetruck];
to_draw.offsets_x = [params.centre_x-trial.dist params.centre_x+trial.dist newstart.x];
to_draw.offsets_y = [params.centre_y params.centre_y newstart.y];

while positionPhase == true
    % check for keyboard press
    [~,~,keyCode] = KbCheck;
    
    % get current mouse pos
    [mx, my, buttons] = GetMouse(params.stimuliScrn);
    
    % Has the mouse been clicked?
    if sum(buttons) > 0 && my <= params.centre_y + box.Rect(3)/2 && my >= params.centre_y - box.Rect(3)/2
        % draw new box co-ordinates
        newstart.x = mx;
        newstart.y = params.centre_y;
        mousedown = true;
    end
    
    % reset locations
    to_draw.offsets_x = [params.centre_x-trial.dist params.centre_x+trial.dist newstart.x];
    to_draw.offsets_y = [params.centre_y params.centre_y newstart.y];
    
    % check for release then record this
    if mousedown == true && sum(buttons) <= 0
        num_clicks = num_clicks + 1;
        clickhist = [clickhist; box.speed_beta(box.speed_order(trial.block),1), trial.block, trial.trial, toc, trial.dist, num_clicks, newstart.x - params.centre_x];
        mousedown = false;
    end
    
    % refresh object locations
    for iter = 1:length(to_draw.objects)
        where_box = CenterRectOnPoint(box.Rect, to_draw.offsets_x(iter), to_draw.offsets_y(iter));
        Screen('DrawTexture', params.stimuliScrn, to_draw.objects(iter), [], where_box);
    end
    
    % Draw dot for mouse cursor
    Screen('DrawDots', params.stimuliScrn, [mx my], 10, mouse.white, [], 2);
    
    % Flip Screen
    Screen('Flip', params.stimuliScrn, anim.vbl + (anim.waitframes - 0.5) * anim.ifi);
    
    % end when spacebar pressed
    if keyCode(keys.spaceKey) && newstart.y == params.centre_y
        positionPhase = false;
        WaitSecs(0.5);
    end
    
    % time taken
    timeelapsed = toc;
    
    % store building location
    build.x = newstart.x;
    build.y = newstart.y;
end
end






%%%%%%%%%%%%%%%%%%%%%%%%
%% Animation function %%
%%%%%%%%%%%%%%%%%%%%%%%%


function [enddata] = anim_function(anim, build, targ, box, newstart, params, enddata, initialx, timeelapsed, trial)
% setup buildings 
if targ.direction == -1
    Left_building = box.png.Burning;
    Right_building = box.png.Building;
elseif targ.direction == 1    
    Left_building = box.png.Building;
    Right_building = box.png.Burning;
end

% setup firetruck 
if strcmp(trial.condition, 'square')
    firetruck = box.png.Fire_truck;
else
    firetruck = box.png.Fire_truck.Front;
end

% list of buildings 
to_draw.objects = [box.png.Placed_truck Left_building Right_building firetruck];
to_draw.offsets_x = [build.x params.centre_x-trial.dist params.centre_x+trial.dist newstart.x];
to_draw.offsets_y = [build.y params.centre_y params.centre_y newstart.y];

while anim.carryon == true
    % check where the target is and move towards it
    if newstart.x < targ.location
        box.movedir = 1;
    elseif newstart.x > targ.location
        box.movedir = -1;
    end
    
    % don't move for first 20 frames
    if anim.move >= box.delay
        newstart.x = newstart.x + box.speed*box.movedir;
        if strcmp(trial.condition, 'truck')
            if box.movedir == 1
                firetruck = box.png.Fire_truck.Right;
            else
                firetruck = box.png.Fire_truck.Left;
            end
        end
    end
    
    % refresh locations 
    to_draw.objects = [box.png.Placed_truck Left_building Right_building firetruck];
    to_draw.offsets_x = [build.x params.centre_x-trial.dist params.centre_x+trial.dist newstart.x];
    to_draw.offsets_y = [build.y params.centre_y params.centre_y newstart.y];
   
    % increase move
    anim.move = anim.move + 1;

    % check if truck is within the range, and if so, change colour
    if newstart.x >= targ.location - max(box.spray)/2 && newstart.x <= targ.location + max(box.spray)/2
        anim.within_range = true;
        anim.success = 1;
        % if true, put out the fire
        if targ.direction == - 1
            Left_building = box.png.Building;
        elseif targ.direction == 1
            Right_building = box.png.Building;
        end
        % if we didn't make, building burns down
    elseif anim.move >= box.movetime + box.delay
        if targ.direction == -1
            Left_building = box.png.Burnt_down;
        elseif targ.direction == 1
            Right_building = box.png.Burnt_down;
        end
    end
    
    % refresh images drawn
    to_draw.objects = [box.png.Placed_truck Left_building Right_building firetruck];
    
    for iter = 1:length(to_draw.objects)
        where_box = CenterRectOnPoint(box.Rect, to_draw.offsets_x(iter), to_draw.offsets_y(iter));
        Screen('DrawTexture', params.stimuliScrn, to_draw.objects(iter), [], where_box);
    end
    
    % flip screen
    anim.vbl = Screen('Flip', params.stimuliScrn, anim.vbl + (anim.waitframes - 0.5) * anim.ifi);
    
    % check are we stopping?
    if (anim.move >= box.movetime + box.delay || anim.within_range == true)
        anim.carryon = false;
        if anim.carryon == false
            for buffer = 1:50
                for iter = 1:length(to_draw.objects)
                    where_box = CenterRectOnPoint(box.Rect, to_draw.offsets_x(iter), to_draw.offsets_y(iter));
                    Screen('DrawTexture', params.stimuliScrn, to_draw.objects(iter), [], where_box);
                end
                anim.vbl = Screen('Flip', params.stimuliScrn, anim.vbl + (anim.waitframes - 0.5) * anim.ifi);
            end
        end
    end
end

% store info
enddata = [enddata; trial.condition_num, box.speed_beta(box.speed_order(trial.block),1), trial.block, trial.trial, trial.dist, timeelapsed, initialx - params.centre_x, build.x - params.centre_x, targ.direction, box.speed, anim.success];
end







%%%%%%%%%%%%%%%%%%%
%% Observe_speed %%
%%%%%%%%%%%%%%%%%%%

function [demo_phase] = observe_speed(params, box, anim, trial, demo_phase)
% wait a short amount of time to avoid overlapping keypresses
WaitSecs(0.5);

% sort out distances
delta = params.delta';
delta = repmat(delta, 1, params.repsestimate)';
delta = delta(:)';

% set trial_num
trial_num = 0;


for dist = delta
    % increase trial_num
    trial_num = trial_num + 1;
    
    % set building up 
    building = box.png.Building;
    
    % get box speed
    [box] = GetSpeed(box, trial);
    
    % to keep loop going
    anim.carryon = true;
    
    % draw the truck
    observe.box_x = params.centre_x;
    observe.box_y = params.centre_y;
    
    % draw target
    % sort side first
    if rand > 0.5
        observe.direction = 1;
    else
        observe.direction = -1;
    end
    
    % offset for this part 
    offsetting = randi(max(params.delta)/2);
    
    % draw the truck
    observe.box_x = params.centre_x + offsetting * -1 * observe.direction;
    observe.box_y = params.centre_y;
    
    % gives location
    observe.target_x = params.centre_x + dist*observe.direction + offsetting * -1 * observe.direction;
    observe.target_y = observe.box_y;
    
    % for animation
    newstartx = observe.box_x;
    
    % setup truck 
    if strcmp(trial.condition, 'square')
        firetruck = box.png.Fire_truck;
    else
        firetruck = box.png.Fire_truck.Front;
    end
    
    % list of buildings
    to_draw.objects = [box.png.Placed_truck building firetruck];

    % sort out positions
    to_draw.offsets_x = [observe.box_x observe.target_x newstartx];
    to_draw.offsets_y = [params.centre_y observe.target_y observe.box_y];

    % set up counter
    anim.move = 0;
    anim.success = 0;
    anim.within_range = false;
    
    % draw this and kbwait
    DrawFormattedText(params.stimuliScrn, ['Press the space bar to start the ' trial.condition], 'center', params.centre_y*0.5);
    
    for iter = 1:length(to_draw.objects)
        where_box = CenterRectOnPoint(box.Rect, to_draw.offsets_x(iter), to_draw.offsets_y(iter));
        Screen('DrawTexture', params.stimuliScrn, to_draw.objects(iter), [], where_box);
    end
    Screen('Flip', params.stimuliScrn, 0);
    KbWait;
    
    % change building to be on fire 
    building = box.png.Burning;
    
    while anim.carryon == true
        % wait box.delay frames as usual
        anim.move = anim.move + 1;
        
        if anim.move >= box.delay
            newstartx = newstartx + box.speed*observe.direction;
            % In truck condition, draw the appropriate truck
            if strcmp(trial.condition, 'truck')
                if observe.direction == -1
                    firetruck = box.png.Fire_truck.Left;
                else
                    firetruck = box.png.Fire_truck.Right;
                end
            end
        end
        
        % Check, are we within range?
        if newstartx >= observe.target_x - max(box.spray)/2 && newstartx <= observe.target_x + max(box.spray)/2
            anim.within_range = true;
            anim.success = 1;
            % if true, put out the fire
            building = box.png.Building;
            % if we didn't make, building burns down
        elseif anim.move >= box.movetime + box.delay
            building = box.png.Burnt_down;
        end
        
        % setup new drawing
        to_draw.objects = [box.png.Placed_truck building firetruck];
        to_draw.offsets = [observe.box_x observe.target_x newstartx];
        
        % draw new screen
        for iter = 1:length(to_draw.objects)
           where_box = CenterRectOnPoint(box.Rect, to_draw.offsets(iter), params.centre_y);
           Screen('DrawTexture', params.stimuliScrn, to_draw.objects(iter), [], where_box);
        end
        
        % flip screen
        anim.vbl = Screen('Flip', params.stimuliScrn, anim.vbl + (anim.waitframes - 0.5) * anim.ifi);
        
        % check if we stop
        if (anim.move >= box.movetime + box.delay || anim.within_range == true)
            anim.carryon = false;
            if anim.carryon == false
                for buffer = 1:50
                    for iter = 1:length(to_draw.objects)
                        where_box = CenterRectOnPoint(box.Rect, to_draw.offsets(iter), params.centre_y);
                        Screen('DrawTexture', params.stimuliScrn, to_draw.objects(iter), [], where_box);
                    end
                    anim.vbl = Screen('Flip', params.stimuliScrn, anim.vbl + (anim.waitframes - 0.5) * anim.ifi);
                end
            end
        end
    end
    
    % make file 
    demo_phase = [demo_phase; box.speed_beta(box.speed_order(trial.block),1), trial_num, dist, anim.success];    
end
end









%%%%%%%%%%%%%%%%%%%
%% Demo Funciton %%
%%%%%%%%%%%%%%%%%%%

function Demo_function(params, box, anim, mouse, trial)
dist = params.delta(round(length(params.delta)/2));

% anim setup
anim.move = 0;
anim.carryon = true;
anim.within_range = false;

newstart.x = params.centre_x;
newstart.y = params.centre_y + 200;

% setup firetruck 
if strcmp(trial.condition, 'square')
    firetruck = box.png.Fire_truck;
else
    firetruck = box.png.Fire_truck.Front;
end

% setup objects to draw
to_draw.objects = [box.png.Building box.png.Building firetruck];
to_draw.offsets_x = [params.centre_x-dist params.centre_x+dist newstart.x];
to_draw.offsets_y = [params.centre_y params.centre_y newstart.y];

% draw text
if strcmp(trial.condition, 'square')
    DrawFormattedText(params.stimuliScrn, 'For every trial, you will see two potential targets (BROWN squares). Each is equally likely to become the target on every trial. Your task is to place the RED square between these two targets to give it the best chance of successfully reaching the true target. \n \n Press space to continue' , 'center', params.centre_y*0.5, [], round(params.x_res/15));    
else
    DrawFormattedText(params.stimuliScrn, 'For every trial, you will see two buildings. Each building is equally likely to catch fire soon. Your job is to park the truck somewhere between the two buildings to give it the best chance of reaching the building that has caught fire. \n \n Press space to continue' , 'center', params.centre_y*0.5, [], round(params.x_res/15));    
end
% draw boxes
for iter = 1:length(to_draw.objects)
   where_box = CenterRectOnPoint(box.Rect, to_draw.offsets_x(iter), to_draw.offsets_y(iter));
   Screen('DrawTexture', params.stimuliScrn, to_draw.objects(iter), [], where_box);
end

Screen('Flip', params.stimuliScrn, 0);
WaitSecs(1.5);
KbWait; 

% this is where the arrow will move
arrowpath.start_x = params.centre_x;
arrowpath.start_y = params.centre_y;
arrowpath.x = [repmat(3,1,round(dist/9)) 0 repmat(3,1,round(dist/9)) 0 repmat(-3,1,round(dist/3/3)) 0 repmat(3,1,round(dist/3/3)) 0 repmat(-3,1,round(dist/9)) 0 0];

while anim.carryon == true
    DrawFormattedText(params.stimuliScrn, ['You can place the ' trial.condition ' by clicking somewhere between the two ' trial.target 's.'], 'center', params.centre_y*0.5, [], round(params.x_res/15));

    % increase anim.move to index into arrowpath
    anim.move = anim.move + 1;
    
    % set mouse position
    arrowpath.move = arrowpath.x(anim.move);
    arrowpath.start_x = arrowpath.start_x + arrowpath.move;
    
    % pause for the clicks and move red box
    if arrowpath.move == 0
        newstart.x = arrowpath.start_x;
        newstart.y = arrowpath.start_y;
        WaitSecs(0.15);
    end    
    
    % update positions
    to_draw.offsets_x = [params.centre_x-dist params.centre_x+dist newstart.x];
    to_draw.offsets_y = [params.centre_y params.centre_y newstart.y];

    % draw boxes moving
    for iter = 1:length(to_draw.objects)
        where_box = CenterRectOnPoint(box.Rect, to_draw.offsets_x(iter), to_draw.offsets_y(iter));
        Screen('DrawTexture', params.stimuliScrn, to_draw.objects(iter), [], where_box);
    end
    
    % draw the mouse at this location
    Screen('DrawDots', params.stimuliScrn, [arrowpath.start_x arrowpath.start_y], 10, mouse.white, [], 2);
    
    % flip screen
    anim.vbl = Screen('Flip', params.stimuliScrn, anim.vbl + (anim.waitframes - 0.5) * anim.ifi);
    
    % after everything, it mouse clicked, pause again
    if arrowpath.move == 0
        WaitSecs(0.15);
    end
    
    if anim.move == length(arrowpath.x)
        anim.carryon = false;
    end
end

% Draw new instruction screen
DrawFormattedText(params.stimuliScrn, 'Once you are happy with the position, press the spacebar to start the trial. \n \n When you are ready, press the space bar to begin the experiment.', 'center', params.centre_y*0.5, [], round(params.x_res/15));
for iter = 1:length(to_draw.objects)
    where_box = CenterRectOnPoint(box.Rect, to_draw.offsets_x(iter), to_draw.offsets_y(iter));
    Screen('DrawTexture', params.stimuliScrn, to_draw.objects(iter), [], where_box);
end
Screen('Flip', params.stimuliScrn, 0);
WaitSecs(1.0);
KbWait;

end

%%%%%%%%%%%%%%%%%%%%%%%
%% Estimate Function %%
%%%%%%%%%%%%%%%%%%%%%%%

function [estimates] = estimate_function(estimates, params, box, anim, keys, trial)
% use this funciton to get estimates of success for
% several distances 

% make list of distances 
delta.even = [];
delta.odd = [];

for ii = 1:length(params.delta)
    if mod(ii, 2) == 1
        delta.even = [delta.even, params.delta(ii)];
    else
        delta.odd = [delta.odd, params.delta(ii)];
    end
end

% create the list 
% probably test each distance twice in each direction?
delta.deltas = [delta.odd fliplr(delta.even) fliplr(delta.odd) delta.even]; 

% setup firetruck 
if strcmp(trial.condition, 'square')
    firetruck = box.png.Fire_truck;
else
    firetruck = box.png.Fire_truck.Front;
end

% setup to_draw
to_draw.objects= [firetruck box.png.Building];
% loop 
for dist = delta.deltas    
    % setup target
    if rand < 0.5
        target.x = params.centre_x + dist * -1;
    else
        target.x = params.centre_x + dist;
    end
    
    % make boxes
    to_draw.offsets = [params.centre_x target.x];
    
    % wait until input
    waiting = true;
    
    % chance to start with
    chance = 5;
    
    % display this part 
    DrawFormattedText(params.stimuliScrn, ['How many times out of 10 would the ' trial.condition ' reach the displayed target? \n Use the up and down arrow keys to raise or lower the value. \n Press the space bar when you have decided.'], 'center', params.centre_y*0.5);
    DrawFormattedText(params.stimuliScrn, [num2str(chance) ' out of 10 times'], 'center', params.centre_y + 100);
    for iter = 1:length(to_draw.objects)
       where_box = CenterRectOnPoint(box.Rect, to_draw.offsets(iter), params.centre_y);
       Screen('DrawTexture',params.stimuliScrn, to_draw.objects(iter), [], where_box)
    end
    % flip it 
    Screen('Flip', params.stimuliScrn, 0);
    WaitSecs(1.0);
    
    % loop through
    while waiting == true
        % check for keyboard press
        [~,~,keyCode] = KbCheck;
        
        % draw some text
        DrawFormattedText(params.stimuliScrn, ['How many times out of 10 would the ' trial.condition ' reach the displayed target? \n Use the up and down arrow keys to raise or lower the value. \n Press the space bar when you have decided.'], 'center', params.centre_y*0.5);
        DrawFormattedText(params.stimuliScrn, [num2str(chance) ' out of 10 times'], 'center', params.centre_y + 100);
        % Draw boxes
        for iter = 1:length(to_draw.objects)
            where_box = CenterRectOnPoint(box.Rect, to_draw.offsets(iter), params.centre_y);
            Screen('DrawTexture',params.stimuliScrn, to_draw.objects(iter), [], where_box)
        end
        % display the screen
        Screen('Flip', params.stimuliScrn, anim.vbl + (anim.waitframes - 0.5) * anim.ifi);
        
        % look for keypresses
        if keyCode(keys.spaceKey)
            WaitSecs(0.2);
            waiting = false;
        elseif keyCode(keys.upKey)
            WaitSecs(0.1);
            chance = chance + 1;
            if chance > 10
                chance = 10;
            end
        elseif keyCode(keys.downKey)
            WaitSecs(0.1);
            chance = chance - 1;
            if chance < 0
                chance = 0;
            end
        end
        
        
    end
    estimates = [estimates; box.speed_beta(box.speed_order(trial.block),1), dist, chance/10];
end
end


%%%%%%%%%%%%%%%%%%%%%%
%% Get Speed of box %%
%%%%%%%%%%%%%%%%%%%%%%

function [box] = GetSpeed(box, trial)
% speed value
speedvalue = betarnd(box.speed_beta(box.speed_order(trial.block),1), box.speed_beta(box.speed_order(trial.block),2))*trial.range + trial.minspeed;

% this part controls it so that in the certain condition
% the box will always travel the same speed
% make box speed
if mod(box.maxspeed,2) == 0
    box.speed = round(speedvalue);
else
    if speedvalue < box.maxspeed/2 + 1
        box.speed = ceil(speedvalue);
    else
        box.speed = round(speedvalue);
    end
end
end


%%%%%%%%%%%%%%%%%%%%%%%%%
%% Confidence Function %%
%%%%%%%%%%%%%%%%%%%%%%%%%

% get estimates of confidence that the truck will make it

function [confidence_data] = confidence_function(confidence_data, params, box, anim, keys, trial, mouse)
% make list of distances 
delta.even = [];
delta.odd = [];

for ii = 1:length(params.delta)
    if mod(ii, 2) == 1
        delta.even = [delta.even, params.delta(ii)];
    else
        delta.odd = [delta.odd, params.delta(ii)];
    end
end

% create the list 
% probably test each distance twice in each direction?
delta.deltas = [delta.odd fliplr(delta.even) fliplr(delta.odd) delta.even]; 

% setup firetruck 
if strcmp(trial.condition, 'square')
    firetruck = box.png.Fire_truck;
else
    firetruck = box.png.Fire_truck.Front;
end

% setup to_draw
to_draw.objects= [firetruck box.png.Building];
% loop 
for dist = delta.deltas    
    % setup target
    if rand < 0.5
        target.x = params.centre_x + dist * -1;
    else
        target.x = params.centre_x + dist;
    end
    
    % make boxes
    to_draw.offsets = [params.centre_x target.x];
    
    % wait until input
    waiting = true;
    
    % display this part 
    % draw some text
    DrawFormattedText(params.stimuliScrn, ['Do you think the ' trial.condition ' would reach the displayed ' trial.target '? \n Press "y" for Yes and "n" for No'], 'center', params.centre_y*0.5);
    % Draw boxes
    for iter = 1:length(to_draw.objects)
        where_box = CenterRectOnPoint(box.Rect, to_draw.offsets(iter), params.centre_y);
        Screen('DrawTexture',params.stimuliScrn, to_draw.objects(iter), [], where_box)
    end
    
    % flip it 
    Screen('Flip', params.stimuliScrn, 0);
    WaitSecs(1.0);
    
    % this loop might be unecessary?
    % loop through
    while waiting == true
        % check for keyboard press
        [~,~,keyCode] = KbCheck;
        
        % draw some text
        DrawFormattedText(params.stimuliScrn, ['Do you think the ' trial.condition ' would reach the displayed ' trial.target '? \n Press "y" for Yes and "n" for No'], 'center', params.centre_y*0.5);
        % Draw boxes
        for iter = 1:length(to_draw.objects)
            where_box = CenterRectOnPoint(box.Rect, to_draw.offsets(iter), params.centre_y);
            Screen('DrawTexture',params.stimuliScrn, to_draw.objects(iter), [], where_box)
        end
        % display the screen
        Screen('Flip', params.stimuliScrn, anim.vbl + (anim.waitframes - 0.5) * anim.ifi);
        
        % look for keypresses
        if keyCode(keys.y)
            response = 1;
            waiting = false;
        elseif keyCode(keys.n)
            response = 0;
            waiting = false;
        end
    end
    
    % reset waiting 
    waiting = true;
        
    % setup mouse
    mouse_x_offset = randi(20)*10;
    if rand < 0.5
        mouse_x_offset = mouse_x_offset * -1;
    end
    SetMouse(params.centre_x + mouse_x_offset, params.centre_y - 200, params.stimuliScrn);
    mousedown = false;
    
    % setup x pos for placed box 
    newstart.x = params.centre_x;
    
    % objects to draw
    to_draw.shapes = [box.judgeline; box.judgebox];
    to_draw.x_pos = [params.centre_x newstart.x];
    to_draw.y_pos = [params.centre_y params.centre_y];
    to_draw.colours = [15 15 15; 180 180 180];
    
    % now draw line to state confidence
    while waiting == true
        % check keyboard
        [~,~,keyCode] = KbCheck;
        
        % get current mouse pos
        [mx, my, buttons] = GetMouse(params.stimuliScrn);
        
        % Has the mouse been clicked?
        if sum(buttons) > 0 && my <= params.centre_y + 250 && my >= params.centre_y + 150
            % draw new box co-ordinates
            newstart.x = mx;
            newstart.y = params.centre_y;
            mousedown = true;
        end
        
        % make some boundaries 
        if newstart.x < params.centre_x - params.centre_x/2
            newstart.x = params.centre_x - params.centre_x/2;
        elseif newstart.x > params.centre_x + params.centre_x/2
            newstart.x = params.centre_x + params.centre_x/2;
        end 
        
        % reset locations
        to_draw.offsets_x = [params.centre_x newstart.x];

        % check for release then record this
        if mousedown == true && sum(buttons) <= 0
            mousedown = false;
        end
        
        % draw truck house setup again? 
        for iter = 1:length(to_draw.objects)
            where_box = CenterRectOnPoint(box.Rect, to_draw.offsets(iter), params.centre_y);
            Screen('DrawTexture',params.stimuliScrn, to_draw.objects(iter), [], where_box)
        end
        
        %Setup shapes
        allRects = nan(4,2);
        for iter = 1:length(to_draw.x_pos)
            allRects(:, iter) = CenterRectOnPoint(to_draw.shapes(iter,:), to_draw.offsets_x(iter), params.centre_y + 210);
        end
        
        % Draw shapes
        Screen('FillRect', params.stimuliScrn, to_draw.colours', allRects);
        
        % Draw dot for mouse cursor
        Screen('DrawDots', params.stimuliScrn, [mx my], 10, mouse.white, [], 2);
        
        if response == 1
            answer = 'Yes';
        else
            answer = 'No';
        end
        
        % ask people are they sure
        DrawFormattedText(params.stimuliScrn, ['You said "' answer '". How sure are you? \n Please use the slider below to indicate how sure you are about your answer.'], 'center', params.centre_y - 300);
        
        % Do some writing either side of the line for
        DrawFormattedText(params.stimuliScrn, 'Not at all sure', (params.centre_x * 0.5) - (params.centre_x*0.2), params.centre_y + 200);
        DrawFormattedText(params.stimuliScrn, 'Completely Sure', params.centre_x * 1.55, params.centre_y + 200);
        
        % show screen
        Screen('Flip', params.stimuliScrn, anim.vbl + (anim.waitframes - 0.5) * anim.ifi);
        
        % end when spacebar pressed
        if keyCode(keys.spaceKey) 
            waiting = false;
            WaitSecs(0.5);
            confidence_score = newstart.x;
        end
    end
      
    % store info 
    confidence_data = [confidence_data; box.speed_beta(box.speed_order(trial.block),1), dist, response, confidence_score-(params.centre_x*0.5)];
        
end
end
