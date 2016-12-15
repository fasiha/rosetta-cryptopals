function [chunks] = partition(x, sizes, overlaps, func, discardMisshapen)
%PARTITION flexibly chunks an array into sub-arrays
%
%  C = PARTITION(X, SIZES) Here, PARTITION is given a multi-dimensional array X
%  and a vector of SIZES (nominally NUMEL(SIZES) == NDIMS(X)), and it will
%  return a cell array containing sub-arrays of the input array whose size is
%  specified by SIZES.
%  >> x = reshape(1 : 20, [4 5]);
%  >> c = partition(x, [2 3]);
%  `c` is a cell array containing 2 by 3 chunks of the origin 4 by 5 input
%  array. Note that the chunk size's second dimension does not evenly divide the
%  input array's size along the second dimension: 5/3 is not whole. By default,
%  PARTITION will return smaller sub-arrays than requested (this behavior can be
%  overridden), so the "rightmost" chunks in `c` are actually 2 by 2 instead of
%  the requested 2 by 3.
%  >> c
%  [2x3 double]    [2x2 double]
%  [2x3 double]    [2x2 double]
%
%  When NUMEL(SIZES) < NDIMS(X), SIZES is padded using SIZE(X). In other words,
%  omitted dimensions will have no partitioning. If NUMEL(SIZES) > NDIMS(X), the
%  extra entries are ignored.
%
%  For each DIM dimension, 1 <= SIZES(DIM) <= SIZE(X, DIM) is enforced, so one
%  may use INF to denote "as big a chunk size as possible", i.e., no chunking
%  along this dimension.
%
%  By default, the partitioned chunks are non-overlapping. However,
%  C = PARTITION(X, SIZES, OVERLAPS) allows one to specify the overlap between
%  chunks. If OVERLAPS is empty or omitted, it defaults to a NDIMS(X)-long
%  vector of zeros. If NUMEL(OVERLAPS) < NDIMS(X) entries, the remaining
%  overlaps default to 0 (no overlap in those trailing dimensions).  If
%  NUMEL(OVERLAPS) > NDIMS(X), the extras are ignored. OVERLAPS(DIM) < SIZE(X,
%  DIM) is enforced. However, there is no lower limit to OVERLAPS' entries:
%  negative overlaps will result in skipped data.
%
%  For example: one has a 5 by 5 array and wishes to have the 2 by 2 chunks from
%  each corner (northeast, southeast, southwest, and northwest corners),
%  skipping the middle row and column.
%  >> x = reshape((1 : 5)' * (1 : 5), 5, 5);
%  >> c = partition(x, [2 2], [-1 -1]);
%  >> c{1,1}
%       1     2
%       2     4
%  >> c{1,2}
%       4     5
%       8    10
%  >> c{2,1}
%       4     8
%       5    10
%  >> c{2,2}
%      16    20
%      20    25
%
%  Sometimes, one wants to run some function on sub-array chunks and obtain the
%  results. One can pass in a function handle:
%  C = PARTITION(X, SIZES, OVERLAPS, FUNC) will return a cell array C whose
%  entries contain the output of function handle FUNC applied to each chunk.
%  FUNC should be a function of a single argument, the chunk itself. If FUNC is
%  omitted or empty, the default is the identity function (@(x) x).
%
%  For example, to find the total sum of each chunk from the previous example:
%  >> partition(x, [2 2], [-1 -1], @(chunk) sum(chunk(:)))
%      [ 9]    [27]
%      [27]    [81]
%
%  As mentioned above, if the requested chunk sizes in SIZES do not evenly
%  divide the data array's size, the edges of the returned cell array will have
%  chunks with smaller size than requested. This can be overridden:
%  C = PARTITION(X, SIZES, OVERLAPS, FUNC, DISCARDMISSHAPEN) for boolean
%  DISCARDMISSHAPEN (default FALSE) will, when TRUE, ensure all elements of the
%  returned cell array have size specified by SIZES by omitting chunks at the
%  edges of X. From the first example above:
%  >> c = partition(x, [2 3])
%      [2x3 double]    [2x2 double]
%      [2x3 double]    [2x2 double]
%  >> c = partition(x, [2 3], [], [], true)
%      [2x3 double]
%      [2x3 double]
%  Notice how in here, with DISCARDMISSHAPEN as TRUE, the second column of C,
%  previously containing 2 by 2 chunks, is discarded.
%
%  Though all examples here have used two-dimensional X, PARTITION is
%  dimensionality-agnostic, and works the same on column vectors as on
%  high-dimensional arrays.
%
%  This function was inspired by Clojure's `partition` and `partition-all`
%  functions [1], [2], but generalized for multidimensional arrays.
%
%  [1] See https://clojuredocs.org/clojure.core/partition
%  [2] Ditto https://clojuredocs.org/clojure.core/partition-all

  %% Fill in omitted or empty arguments
  if ~exist('overlaps', 'var') || isempty(overlaps)
    overlaps = zeros(1, ndims(x));
  end

  if ~exist('func', 'var') || isempty(func)
    func = @(x) x;
  end

  if ~exist('discardMisshapen', 'var') || isempty(discardMisshapen)
    discardMisshapen = false;
  end

  %% Correct `sizes` and `overlaps`
  if numel(sizes) > ndims(x)
    % trim `sizes`
    sizes = sizes(1 : ndims(x));
  elseif numel(sizes) < ndims(x)
    % pad `sizes` with sizes of dimensions
    fixed = size(x);
    fixed(1 : numel(sizes)) = sizes;
    sizes = fixed;
  end
  % make sure `sizes` doesn't contain elements > size of that dimension
  sizes = min(sizes, size(x));
  % make sure `sizes` doesn't contain <= 0
  sizes = max(sizes, 1);

  if numel(overlaps) > ndims(x)
    overlaps = overlaps(1 : ndims(x));
  elseif numel(overlaps) < ndims(x)
    fixed = size(x) * 0;
    fixed(1 : numel(overlaps)) = overlaps;
    overlaps = fixed;
  end
  % Require for each `overlap` in `overlaps`: `-inf < overlap < sizes`
  overlaps = min(overlaps, sizes - 1);

  %% For each dimension of `x`, make a vector indicating the start of each chunk
  % Sure, `sizes` and `overlaps` are what we like to think about when we think
  % about partitioning arrays, but really, `sizes - overlaps`, i.e., the hop
  % sizes, are very important too. In the default non-overlapping case, the hop
  % size IS the chunk size. In the overlapping case, the hop size is smaller
  % than the chunk size. In the skip case (negative overlap), the hop size is
  % bigger than chunk size.
  %
  % So for each dimension, make a vector of indexes of each chunk. This is what
  % `chunkStarts` is. `chunkStarts{DIM}[i]` gives you the start index (in
  % terms of `x`) for the `i`th chunk along the `DIM`th dimension. This won't be
  % too memory-intensive, since we're just storing the starts, and storing each
  % dimension separately.

  hops = sizes(:)' - overlaps(:)';
  chunkStarts = cell(1, ndims(x));
  for dim = 1:ndims(x)
    if discardMisshapen
      chunkStarts{dim} = 1 : hops(dim) : (size(x, dim) - sizes(dim) + 1);
    else
      chunkStarts{dim} = 1 : hops(dim) : size(x, dim);
    end
  end

  %% Get the Cartesian product of all `chunkStarts`.
  % This will take more memory because we generate all combinations of
  % `chunkStarts`. `allcomb` here returns a tall array, whose width is
  % `ndims(x)`. `allStarts(i,:)` gives you the multidimensional index into `x`
  % of the start of the `i`th chunk. Note we've rasterized the dimensions here,
  % don't worry, we'll resurrect them later.

  allStarts = allcomb(chunkStarts{:}, 'matlab');

  %% For each dimension of `x`, make 0-start vectors to be added to start idx
  % So, a row of `allStarts` tells us the index into `x` to start a given chunk.
  % I.e., if `allStarts(end, :)` was `[30 40]`, then `x(30, 40)` would be the
  % first element of the last chunk returned. But to get the rest of the chunk,
  % we need `x(30:31, 40:41)`, say, for 2 by 2 chunks. Rather than take up a ton
  % of memory storing each dimension's indexing vector (like `30:31` or `40:41`
  % just now), we'll use an indexing trick. Note how `x(30:31, 40:41)` is the
  % same as `x(30 + [0:1], 40 + [0:1])`. So, `summers` contain `ndims(x)` number
  % of 0-start vectors that can be added to a given index to give a complete
  % vector capable of indexing into a given dimension of `x`.

  summers = cell(1, ndims(x));
  for dim = 1:ndims(x)
    summers{dim} = 0 : (sizes(dim) - 1);
  end

  %% Build the return cell array: extract chunks
  % So now we have the indexes corresponding to the *first* element of each
  % chunk `allStarts`, and we also have index vectors that make it easy to go
  % from a single index to a range of indexes. So all we need is to loop over
  % each chunk's starting index, in `allStarts`, and extract that chunk from
  % `x`. That indexing is what `subsref` does: that's the function version of
  % array indexing, and we need its complication because this `partition`
  % function is completely agnostic to the dimension of `x`: `x` can be a
  % simple column vector or a five-dimensional monster. So see `help subsref`,
  % but basically, it needs a struct containing the indexes in each dimension.
  % We can build those by combining `allStarts` and `summers`: we do this via
  % `arrayfun`.
  %
  % We have a tiny bit of complication, with the `cap` function handle too,
  % because the vectors in `summers` don't consider the possibility that they
  % might index beyond the end of an array. So `cap` just lets us limit the
  % sizes of the index. If Matlab or `subsref` had a mechanism wherein it would
  % ignore too-big indexes, we wouldn't need this `cap`.
  %
  % Note that throughout this process, we have avoided any egregious memory
  % consumption. The dimensions have been kept separate, and the biggest array
  % we created, `allStarts` via `allcomb`, is `total number of chunks` by
  % `ndims(x)`. If `func` reduces each chunk to a scalar, the return cell
  % `chunks` could be much smaller in memory than the input `x`. If the default
  % `func`, the identity function, is used, then sure, you'll use more memory.
  % With 0-overlap, `chunks` will use a bit more RAM than `x`. With a lot of
  % overlap, you'll need proportionately more RAM.

  cap = @(x, maxVal) x(x <= maxVal);
  chunks = cell(size(allStarts, 1), 1);
  subsStruct = struct('type', '()', 'subs', {{}});
  for row = 1 : size(allStarts, 1)
    starts = allStarts(row, :);
    subs = arrayfun(@(i) cap(starts(i) + summers{i}, size(x, i)), ...
                    1:ndims(x), ...
                    'un', false);
    subsStruct.subs = subs;
    chunks{row} = func(subsref(x, subsStruct));
  end

  %% Un-rasterize chunks
  % `allcomb()`, which we used to generate `allStarts` array, rasterized the
  % starting indexes, so `chunks` right now is a column cell array. We can
  % readily reshape it (a very cheap operation) to have `ndims(x)` dimensions.

  chunks = reshape(chunks, cellfun(@length, chunkStarts));
  % if it hadn't been for the `'matlab'` argument to 'allComb()`, we'd need:
  % >> chunks = reshape(chunks, fliplr(cellfun(@length, chunkStarts)))';
  % It's a C vs Fortran/Matlab ordering thing.

end



% In order to be fully self-contained, this function includes a local copy of
% `allcomb` function, a BSD-licensed submission to the Mathworks File Exchange
% [3] by Jos.
%
% [3] https://www.mathworks.com/matlabcentral/fileexchange/10064
function A = allcomb(varargin)

% ALLCOMB - All combinations
%    B = ALLCOMB(A1,A2,A3,...,AN) returns all combinations of the elements
%    in the arrays A1, A2, ..., and AN. B is P-by-N matrix is which P is the product
%    of the number of elements of the N inputs. This functionality is also
%    known as the Cartesian Product. The arguments can be numerical and/or
%    characters, or they can be cell arrays.
%
%    Examples:
%       allcomb([1 3 5],[-3 8],[0 1]) % numerical input:
%       % -> [ 1  -3   0
%       %      1  -3   1
%       %      1   8   0
%       %        ...
%       %      5  -3   1
%       %      5   8   1 ] ; % a 12-by-3 array
%
%       allcomb('abc','XY') % character arrays
%       % -> [ aX ; aY ; bX ; bY ; cX ; cY] % a 6-by-2 character array
%
%       allcomb('xy',[65 66]) % a combination
%       % -> ['xA' ; 'xB' ; 'yA' ; 'yB'] % a 4-by-2 character array
%
%       allcomb({'hello','Bye'},{'Joe', 10:12},{99999 []}) % all cell arrays
%       % -> {  'hello'  'Joe'        [99999]
%       %       'hello'  'Joe'             []
%       %       'hello'  [1x3 double] [99999]
%       %       'hello'  [1x3 double]      []
%       %       'Bye'    'Joe'        [99999]
%       %       'Bye'    'Joe'             []
%       %       'Bye'    [1x3 double] [99999]
%       %       'Bye'    [1x3 double]      [] } ; % a 8-by-3 cell array
%
%    ALLCOMB(..., 'matlab') causes the first column to change fastest which
%    is consistent with matlab indexing. Example:
%      allcomb(1:2,3:4,5:6,'matlab')
%      % -> [ 1 3 5 ; 1 4 5 ; 1 3 6 ; ... ; 2 4 6 ]
%
%    If one of the arguments is empty, ALLCOMB returns a 0-by-N empty array.
%
%    See also NCHOOSEK, PERMS, NDGRID
%         and NCHOOSE, COMBN, KTHCOMBN (Matlab Central FEX)

% Tested in Matlab R2015a
% version 4.1 (feb 2016)
% (c) Jos van der Geest
% email: samelinoa@gmail.com

% History
% 1.1 (feb 2006), removed minor bug when entering empty cell arrays;
%     added option to let the first input run fastest (suggestion by JD)
% 1.2 (jan 2010), using ii as an index on the left-hand for the multiple
%     output by NDGRID. Thanks to Jan Simon, for showing this little trick
% 2.0 (dec 2010). Bruno Luong convinced me that an empty input should
% return an empty output.
% 2.1 (feb 2011). A cell as input argument caused the check on the last
%      argument (specifying the order) to crash.
% 2.2 (jan 2012). removed a superfluous line of code (ischar(..))
% 3.0 (may 2012) removed check for doubles so character arrays are accepted
% 4.0 (feb 2014) added support for cell arrays
% 4.1 (feb 2016) fixed error for cell array input with last argument being
%     'matlab'. Thanks to Richard for pointing this out.

narginchk(1,Inf) ;

NC = nargin ;

% check if we should flip the order
if ischar(varargin{end}) && (strcmpi(varargin{end},'matlab') || strcmpi(varargin{end},'john')),
    % based on a suggestion by JD on the FEX
    NC = NC-1 ;
    ii = 1:NC ; % now first argument will change fastest
else
    % default: enter arguments backwards, so last one (AN) is changing fastest
    ii = NC:-1:1 ;
end

args = varargin(1:NC) ;
% check for empty inputs
if any(cellfun('isempty',args)),
    warning('ALLCOMB:EmptyInput','One of more empty inputs result in an empty output.') ;
    A = zeros(0,NC) ;
elseif NC > 1
    isCellInput = cellfun(@iscell,args) ;
    if any(isCellInput)
        if ~all(isCellInput)
            error('ALLCOMB:InvalidCellInput', ...
                'For cell input, all arguments should be cell arrays.') ;
        end
        % for cell input, we use to indices to get all combinations
        ix = cellfun(@(c) 1:numel(c), args,'un',0) ;

        % flip using ii if last column is changing fastest
        [ix{ii}] = ndgrid(ix{ii}) ;

        A = cell(numel(ix{1}),NC) ; % pre-allocate the output
        for k=1:NC,
            % combine
            A(:,k) = reshape(args{k}(ix{k}),[],1) ;
        end
    else
        % non-cell input, assuming all numerical values or strings
        % flip using ii if last column is changing fastest
        [A{ii}] = ndgrid(args{ii}) ;
        % concatenate
        A = reshape(cat(NC+1,A{:}),[],NC) ;
    end
elseif NC==1,
    A = args{1}(:) ; % nothing to combine

else % NC==0, there was only the 'matlab' flag argument
    A = zeros(0,0) ; % nothing
end

end
