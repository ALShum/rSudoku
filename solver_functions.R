solve_sudoku = function(puzzle) {
  
  #' tracks candidate values for each cell
  cand = lapply(1:81, function(x){
    if(puzzle[x]==0) 1:9
    else 0
  })
  dim(cand) = c(9,9)
  
  #' Prunes constraint; if only one possibility fills into puzzle
  #' repeats until convergence
  prune = function(puzzle, cand) {
    
    repeat {
      changed = FALSE
      
      for(row in 1:9) {
        for(col in 1:9) {
          if(!all(cand[[row, col]] == 0)) {
            #row constraint
            cand[[row, col]] = setdiff(cand[[row, col]], puzzle[row, ])
            #column constraint
            cand[[row, col]] = setdiff(cand[[row, col]], puzzle[, col])
            
            #3x3 box constraint
            xmin = col - ((col - 1) %% 3)
            xmax = col + ( 3 - (((col - 1) %% 3) + 1) )
            ymin = row - ((row - 1) %% 3)
            ymax = row + ( 3 - (((row - 1) %% 3) + 1) )
            cand[[row, col]] = setdiff(cand[[row, col]], puzzle[ymin:ymax, xmin:xmax])
          }
          
          #fill in if only one possibility
          if(length(cand[[row, col]]) == 1 & all(cand[[row, col]] != 0)) {
            puzzle[row, col] = cand[[row, col]]
            cand[[row, col]] = 0
            changed = TRUE
          }
          
          #empty sets get set to 0
          if(length(cand[[row, col]]) == 0) cand[[row, col]] = 0
        }
      }
      
      if(!changed) break()
    }
    
    list(puzzle, cand)
  }
  
  #' Finds cell with fewest viable candidates
  find_easiest = function(cand) {
    easiest = which.min(lapply(cand, function(x){
      if(!any(x == 0)) length(x)
      else NA
    }))
    
    easiest
  }
  
  #' depth first search solution
  dfs = function(puzzle, cand) {
    #prune
    pruned = prune(puzzle, cand)
    puzzle = pruned[[1]]
    cand = pruned[[2]]
    
    #failure
    if(sum(as.vector(puzzle == 0) & sapply(cand, function(x){all(x == 0)})) > 0) return(NULL)
    
    #success
    if(sum(puzzle == 0) == 0) return(puzzle)
    
    cell_location = find_easiest(cand)
    candidates = cand[[cell_location]]
    
    for(x in candidates) {
      puzzle[cell_location] = x
      cand[[cell_location]] = 0
      
      ans = dfs(puzzle, cand)
      if(!is.null(ans)) return(ans)
      else candidates = setdiff(candidates, x)
    }
    
    return(NULL)
  }
  
  dfs(puzzle, cand)
}


puzzle = c(8,0,0,0,0,0,0,0,0,
           0,0,7,5,0,0,0,0,9,
           0,3,0,0,0,0,1,8,0,
           0,6,0,0,0,1,0,5,0,
           0,0,9,0,4,0,0,0,0,
           0,0,0,7,5,0,0,0,0,
           0,0,2,0,7,0,0,0,4,
           0,0,0,0,0,3,6,1,0,
           0,0,0,0,0,0,8,0,0)
puzzle = matrix(puzzle, nrow = 9)

print(puzzle)
solve_sudoku(puzzle)