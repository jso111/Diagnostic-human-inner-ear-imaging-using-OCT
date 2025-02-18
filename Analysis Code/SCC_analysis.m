% Steps to process the scc images 
% 1. Load images
% 2. crop a roi
% 3. select the top of the scc;
% 5. select the bottom of the scc;
% 6. select the hydrop membrane;
% 7. fitting
% 8. get the E/P ratio..
clc 
clear all
close all
addpath '..\ImageFitting'



%% select a Bscan 
Bscan1 = imread('C:\NewAutomationMeasurementTest\Images + results\AN3\PSCC\Bscan_1.tiff'); % vertical LSCC

% 2. crop a roi, including the scc area
[~, rect] = imcrop(Bscan1);
rect = round(rect);
Im_sub = imcrop(Bscan1,rect);
Im_sub = rescale(Im_sub);
figure(1),imshow(Im_sub,[]);

%%%%%%%%%%%%%%%%%%%%%%%%%% scc area measurement %%%%%%%%%%%%%%%%%%%%%%%%%%%
%% select the bottom of the scc
% if can see the bottom part, select any points (using free draw tool draw 
% a range). Then will auto select all bright spots in the range as the 
% bottom points...
h = figure(2);
imshow(Im_sub,[0,1-0.1]);
hFH = drawfreehand();
binarlImage = hFH.createMask();
Im_scc_b = binarlImage.*Im_sub;
mask_1 = imbinarize(Im_scc_b); % bottom scc mask
[y1,x1] = find(mask_1);
close(h)

%% top scc selection
% draw a box including the top scc part. 
% after edge detection, select the edge and exclude non-edge parts..
% eliipse fitting
[~, rect1] = imcrop(Im_sub);
rect1 = round(rect1);
Im_scc_t = imcrop(Im_sub,rect1);
mask_2 = imbinarize(Im_scc_t); % bottom scc mask
se1 = strel('disk',2,8);
mask_2 = imdilate(mask_2,se1);
edges1 = edge(mask_2,'canny');
edges1_1 = zeros(size(Im_sub));
edges1_1(rect1(2):rect1(2)+rect1(4),rect1(1):rect1(1)+rect1(3)) = edges1;

% fit the scc
[~, rect2] = imcrop(edges1_1);
rect2 = round(rect2);
edges1_2 = imcrop(edges1_1,rect2); % get the edges
edges1_3 = zeros(size(Im_sub));
edges1_3(rect2(2):rect2(2)+rect2(4),rect2(1):rect2(1)+rect2(3)) = edges1_2;
[~, rect3] = imcrop(edges1_3);
rect3 = round(rect3);
edges1_3(rect3(2):rect3(2)+rect3(4),rect3(1):rect3(1)+rect3(3)) = 0;
[y2,x2] = find(edges1_3); % top scc part
y = [y1;y2];
x = [x1;x2];

figure(2),imshow(Im_sub,[]);hold on
ellipse_t = fit_ellipse( x,y,2 );
scatter(x,y,10,[0,161/255,241/255]); % fitting points
% scatter(x3,y3,10,[1 187/255 0]); % endolymph area points
hold off
ellipse_area = pi.*(ellipse_t.long_axis/2).*(ellipse_t.short_axis/2);
fprintf('ellipse area = %.4f pixel\x00B2\n',ellipse_area);

%% select the hydrop membrane
% free draw the hydrop area to include the boundary.
% filter
Im_filt1 = imgaussfilt(Im_sub,0.8); % based on images, to select a proper filter..
% Im_filt1 = medfilt2(Im_filt1,[5,5]);
h = figure(3);
imshow(Im_filt1,[]);
hFH = drawfreehand(); % draw the hydrop membrane
binarlImage = hFH.createMask();
Im_endoly1 = binarlImage.*Im_filt1;
close(h)
edges11 = edge(Im_endoly1,'canny');
h = figure(4);
imshow(edges11,[]);
hFH = drawfreehand(); % draw the hydrop membrane
binarlImage = hFH.createMask();
Im_endoly1 = binarlImage.*ones(size(Im_sub));
close(h)

%%%%%%%%%%%%%%%%%%% measure the endolymph %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mask3 = imbinarize(Im_endoly1);
mask3 = imfill(mask3,"holes");
mask5 = imcomplement(mask3);
[~, rect4] = imcrop(mask5);
rect4 = round(rect4);
mask5 = imcrop(mask5,rect4); % get the edges
gray_invt_oimg = imcomplement(mask5);
[L1,num1] = bwlabel(gray_invt_oimg,8);
stats1 = regionprops(L1,'Area');
Num = 1;
[~,index] = sort([stats1.Area],'descend');
if length(stats1)<=Num
    bw1_3 = L1;
else
    bw1_3 = ismember(L1,index(1:Num));
end
bw1_2 = imfill(bw1_3,"holes");
se3 = strel('disk',1,4);
bw1_3 = imclose(bw1_2,se3);
Im_endo1 = zeros(size(Im_sub));
Im_endo1(rect4(2):rect4(2)+rect4(4),rect4(1):rect4(1)+rect4(3)) = bw1_3;
Endolymph_area = bwarea(Im_endo1);
fprintf('Endolymph_area = %.4f pixel\x00B2\n',Endolymph_area);
[y3,x3] = find(Im_endo1); % endolymph area coordinates
% figure(6),imshow(Im_sub,[]);
% hold on
% scatter(x3,y3,10,'red');
% hold off
boundaries = bwboundaries(Im_endo1);
f7 = figure(7);
imshow(Im_sub,[]);hold on
ellipse_t = fit_ellipse( x,y,7 );
scatter(x,y,10,[0,161/255,241/255]); % fitting points
scatter(x3,y3,14,[1 187/255 0]); % endolymph area points

hold off
f7.Position(3:4) = [600 600];

%% calculate ratio
area_Endolymph = Endolymph_area;
area_Perilymph = ellipse_area - area_Endolymph;
ratio = area_Endolymph/area_Perilymph;
fprintf('E/P ratio = %.4f \n',ratio);

