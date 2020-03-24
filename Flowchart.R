? DiagrammeR


DiagrammeR("graph TB;
    A(Select campaign by Code_Product, Code_Objective, Code_RecruitingPeriod)-->B[Squared];
    B(Tidy this data by converting to quarters)---C{Rhombus!};
    C(Summarize all of the key metrics)-->D>Show quick summarized table];
    C-->E(Get start and end dates of each campaign);
           E-->F(Segregate into audiences. );
           F-->G((Audience A));
           F-->H((Audience B));
           F-->I((Audience C));
           G-->J((Audience A contribution of web visits, CTR, expense));
           H-->J((Audience B contribution of web visits, CTR, expense));
           I-->J((Audience C contribution of web visits, CTR, expense. Who is most valuable? Who is a drain? Any ads in particular that out perform?));
           
           
           
           ")

