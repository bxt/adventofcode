
function [] = main()
    part1()
    part2()
end

function time = find_good_time(disc_ids, disc_sizes, disc_start)
    disc_vector = disc_ids + disc_start;
    time = 0;
    while any(mod(time + disc_vector, disc_sizes))
        time = time + 1;
    end
end

function [] = part1()
    [disc_ids, disc_sizes, disc_start] = importfile('input.txt');
    find_good_time(disc_ids, disc_sizes, disc_start)
end

function [] = part2()
    [disc_ids, disc_sizes, disc_start] = importfile('input.txt');
    disc_ids = [disc_ids; 7];
    disc_sizes = [disc_sizes; 11];
    disc_start = [disc_start; 0];
    find_good_time(disc_ids, disc_sizes, disc_start)
end

function [disc_ids, disc_sizes, disc_start] = importfile(filename, startRow, endRow)
    delimiter = {' positions; at time=0, it is at position ','Disc #',' has ','.'};
    if nargin<=2
        startRow = 1;
        endRow = inf;
    end

    formatSpec = '%*s%u%u%u%[^\n\r]';

    fileID = fopen(filename,'r');

    dataArray = textscan(fileID, formatSpec, endRow(1)-startRow(1)+1, 'Delimiter', delimiter, 'HeaderLines', startRow(1)-1, 'ReturnOnError', false);
    for block=2:length(startRow)
        frewind(fileID);
        dataArrayBlock = textscan(fileID, formatSpec, endRow(block)-startRow(block)+1, 'Delimiter', delimiter, 'HeaderLines', startRow(block)-1, 'ReturnOnError', false);
        for col=1:length(dataArray)
            dataArray{col} = [dataArray{col};dataArrayBlock{col}];
        end
    end

    fclose(fileID);

    disc_ids = dataArray{:, 1};
    disc_sizes = dataArray{:, 2};
    disc_start = dataArray{:, 3};
end

