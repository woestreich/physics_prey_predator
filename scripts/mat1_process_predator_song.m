% Blue_Fin_DailyCI_diel.m: 
% Compute daily call indices for blue whales from the CI basis file

% 1. Daily call index
clear all; close all;

% Load data basis for CI computations
load '../data_raw/CIbasis.mat'

% Call index frequencies (background, peak, peak, background)
cif.blue = [37 42 43 50]; cif.fin = [12 20 21 34];
[q,ia,ib] = intersect(cif.blue,freq);  % index into psd frequency axis

minPC = 0.95; % minimum daily percent recording coverage
minCI = 1.01; % minimum daily CI

ndays = size(Q,2); % number of days in time series
nfreq = length(freq); % number of 1-Hz frequency bands in power spectra
for D = 1:ndays;
    % time and daily recording coverage
    time(D,1) = Q{D}.time;
    dailysec(D,1) = 5*sum(Q{D}.ct);  % for screening later
    % daily overall mean psd
    psd(:,D) = sum(Q{D}.sm,2)/sum(Q{D}.ct); 
    % by solar elevation category
    for E = 1:3; psde(:,D,E) = Q{D}.sm(:,E)/Q{D}.ct(E); end
end
disp(['CI data available for ' int2str(ndays) ' days between ' ...
    datestr(min(time)) ' and ' datestr(max(time))]);

% screen days based on recording coverage
set(0,'DefaultAxesFontsize',14)
figure(1); clf; 
subplot(211); plot(time,dailysec,'k.'); datetick('x'); axis tight; 
hold on; title('Exclusion based on temporal coverage')
ylabel('Seconds of recording per day')
xcl = find(dailysec < minPC * 86400); nxcl = numel(xcl);
plot(time(xcl),dailysec(xcl),'ro')
pctxcl = 100*nxcl/ndays;
disp(['Excluding ' int2str(nxcl) ' days (' int2str(pctxcl) '%) for insufficient sampling (< ' int2str(minPC*100) '%)'])
time(xcl) = []; dailysec(xcl) = [];
psd(:,xcl) = []; psde(:,xcl,:) = [];

% screen days based on daily CI
% Call indices
ci.blue = mean(psd(ib([2 3]),:)) ./ mean(psd(ib([1 4]),:));
subplot(212);
plot(time,ci.blue,'k.','color',[.7 .7 .7]); datetick('x'); axis tight; 
hold on; title('Exclusion based on daily CI < 1.01')
ylabel('CI')
xcl = find(ci.blue < minCI);
plot(time(xcl),ci.blue(xcl),'ro')
time(xcl) = []; dailysec(xcl) = []; 
psd(:,xcl) = []; psde(:,xcl,:) = [];
ci.blue(xcl) = [];

% screen based on month 
dv = datevec(time); xcl = find(dv(:,2) >= 3 & dv(:,2) <= 6);
time(xcl) = []; dailysec(xcl) = [];
psd(:,xcl) = []; psde(:,xcl,:) = [];
ci.blue(xcl) = [];

% Call indices by solar elevation bin
CI = []; 
for E = 1:3;
    cpsd = squeeze(psde(:,:,E));
    CI.blue(E,:) = mean(cpsd(ib([2 3]),:)) ./ mean(cpsd(ib([1 4]),:));
end

% Place call index overall for day in the CI.blue array
CI.blue(4,:) = ci.blue;

% Plot CI 
figure(2); clf; 
plot(time,CI.blue(4,:),'k.','markersize',16); hold on; 
plot(time,CI.blue(1,:),'b+'); plot(time,CI.blue(3,:),'ro');
legend('Daily','night','day'); ylabel('CI')
datetick('x'); axis tight; 
xl = get(gca,'Xlim'); plot(xl,[1 1],'k--')

% Compute CI ratio and difference metrics.
% All daily mean CI values are > 1.01, however night and day values
% individually fall below 1.01, and even below 1, which would be
% problematic for a night:day ratio that subtracts 1 before computing
% the ratio. It is reasonable to find values < 1 and set them to 1.0001
% (effectively no signal above backgroun).
idx = find(CI.blue < 1); if ~isempty(idx); CI.blue(idx) = 1.0001; end
% Day:night ratio and difference
ndr = (CI.blue(1,:)-1) ./ (CI.blue(3,:)-1);
ndd = CI.blue(1,:) - CI.blue(3,:);

% Plot night:day ratio and difference.
figure(3); clf; 
subplot(211); plot(time,ndr,'b.','markersize',16); datetick('x'); axis tight;
ylabel('CI night:day ratio')
subplot(212); plot(time,ndd,'b.','markersize',16); datetick('x'); axis tight;
ylabel('CI night–day difference')


% save for evaluation in R
date = time;
blue_ci = CI.blue(1,:)';
blue_ci_ratio = ndr';

start_date = datenum('2022-01-01');  % Jan 1, 2022
end_date = datenum('2024-02-29');    % Feb 29, 2024
valid_dates = (date >= start_date) & (date <= end_date);

filtered_date = date(valid_dates);
filtered_date_str = datestr(filtered_date, 'yyyy-mm-dd HH:MM:SS');
filtered_blue_ci = blue_ci(valid_dates);
filtered_blue_ci_ratio = blue_ci_ratio(valid_dates);

% Write the output matrix to a CSV file
ci_bwo = table(filtered_date_str, filtered_blue_ci, filtered_blue_ci_ratio);

% Set the column names for the table
ci_bwo.Properties.VariableNames = {'date', 'blue_ci', 'blue_ci_ratio'};

% Write the table to a CSV file (with headers)
writetable(ci_bwo, '../data_processed/song_daily.csv');




% time = time(:); ndr = ndr(:); ndd = ndd(:); yref = NaN*time;
% yz = 2015:2023;
% for Y = 1:length(yz);
%     s = datenum([yz(Y) 7 1]); e = datenum([yz(Y)+1 3 1]);
%     idx = find(time >=s & time <= e);
%     yref(idx) = Y;
% end
% 
% otime = sdn2utc(time);
% save ../outputs/CIratio4R.mat otime yref ndr ndd


% 
% 
% % 2. BWO 2022
% BWO2022focus = 0;
% if BWO2022focus
% 
%     close all
% 
%     figure(1); clf; 
%     kxl = [datenum([2022 8 17; 2022 12 1])]';
%     % kxl = [datenum([2022 8 1; 2022 12 1])]';
%     k = find(time >= kxl(1) & time <= kxl(2));
% 
%     subplot(221); 
%     plot(time(k),CI.blue(3,k),'r.',time(k),CI.blue(1,k),'bo');
%     legend('day','night'); datetick('x'); axis tight;
%     ylabel('CI')
%     subplot(222); plot(time(k),ndr(k),'b.','markersize',25); 
%     datetick('x'); set(gca,'Xlim',kxl);
%     set(gca,'Ylim',[0 3])
%     title('night:day ratio'); hold on; yl = get(gca,'Ylim');
%     plot(datenum([2022 8 17])+[0 0],yl,'r--')
%     plot(datenum([2022 10 1])+[0 0],yl,'r--')
%     subplot(224); plot(time(k),ndd(k),'b.','markersize',16); 
%     datetick('x'); set(gca,'Xlim',kxl);
%     title('night–day difference'); hold on; yl = get(gca,'Ylim');
%     plot(datenum([2022 8 17])+[0 0],yl,'r--')
%     plot(datenum([2022 10 1])+[0 0],yl,'r--')
% 
%     subplot(223)
%     plot(time(k),ndr(k),'b.','markersize',25); 
%     datetick('x'); set(gca,'Xlim',kxl);
%     set(gca,'Ylim',[0 3],'fontsize',12)
%     hold on; yl = get(gca,'Ylim');
%     % plot(datenum([2022 8 17])+[0 0],yl,'r--','linewidth',1)
%     plot(datenum([2022 10 1])+[0 0],yl,'r--','linewidth',1)
%     ylabel('Daily blue whale CI night:day ratio')
% 
%     C.time = time(k); C.CI = [CI.blue(4,k)]'; C.ndr = [ndr(k)]';
%     save ../outputs/blueCI_2022 C; 
% 
%     return
% end



