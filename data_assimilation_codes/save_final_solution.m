 function  [fnl_sln_file_name1st,store_sln_every] = save_final_solution(Year,DOY,Time,Len_Assim_win,fnl_sln_file_name1st,...
                                                                                Final_solution_flder,slashx,Xanalysis) 
                                                                            
 % This function saves the solution in the final final folder 
 % By Nicholas Ssessanga while at Chungnam National University
 % 2019-july-04

                     % get name for the final segment processed
                     [fnl_sln_file_name_end]  =  formulate_solution_name(Year,DOY,Time);
                     % combine the first and final segment to make one name
                     % final file name 
                     fnl_sln_file_namei = ['Anal_', fnl_sln_file_name1st,'_',fnl_sln_file_name_end];

        
                     [fnl_sln_file_name1st]  =  formulate_solution_name(Year,DOY,Time);
                     
                     
                     % reset the store counter  
                     store_sln_every = 0;
                     
                     RE = ['save ',Final_solution_flder,slashx,fnl_sln_file_namei, ' Xanalysis' ];
                     eval (RE)

                    % save dump all the solutions in that segment  to a folder %                                        
                    disp('Solution saved in ....')
                    disp([Final_solution_flder,slashx, fnl_sln_file_namei])
                    
                    