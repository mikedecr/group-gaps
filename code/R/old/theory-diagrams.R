
# install.packages("DiagrammeR")
library("DiagrammeR")



# party vote <- mechanisms
{
  DiagrammeR("
           graph LR;
           M(Mobilization of Party Supporters) --> V(Votes for Party);
           P(Persuasion of Other-Party Supporters) --> V
           ", fill = "white")
}


# vote margin <- diff.parties <- mechanisms
{
  DiagrammeR("
             graph LR;
             D(Democratic Votes) --> V(Net Democratic Votes);
             R(Republican votes) --> V;
             MD(Mobilization of Democrats) --> D;
             PD(Persuasion of Republicans) --> D ;
             MR(Mobilization of Republicans) --> R;
             PR(Persuasion of Democrats) --> R
             ")
}


# well, if you difference mechanisms across party, you can express an outcome as a function of "partisan advantages" in each mechanism
# vote margin <- mechanisms <- diff.parties
{
  DiagrammeR("
             graph LR;
             M(Net Democratic <br> Mobilization Advantage) --> V(Net Democratic Votes);
             P(Net Democratic <br> Persuasion Advantage) --> V;
             MD(Mobilization of Democrats) --> M;
             MR(Mobilization of Republicans) --> M ;
             PD(Persuasion of Democrats) --> P;
             PR(Persuasion of Republicans) --> P
             ")
}

# by extension, the gender gap is a function of these margins among men and women.
# with margins measured as proportions, this is the "usual" gender gap
# with margins measured as raw votes, we get a gap in vote count
# gap <- Wmargin + Mmargin
{
  DiagrammeR("
    graph LR;
    W(Net Democratic Votes, Women) --> G(Net Gender Gap);
    M(Net Democratic Votes, Men) --> G
             ")
}


# we can distribute this logic throughout, finding gender gaps in each mechanism
# gap <- gender margins <- gender mechanisms
{
  mermaid("
             graph LR;
             W(Net Democratic Votes, Women) --> G(Net Gender Gap <br> or <br> Net Democratic Votes);
             M(Net Democratic Votes, Men) --> G;
             Wmob(Net Democratic <br> Mobilization, Women) --> W;
             Wper(Net Democratic <br> Persuasion, Women) --> W;
             Mmob(Net Democratic <br> Mobilization, Men) --> M;
             Mper(Net Democratic <br> Persuasion, Men) --> M
             ")
}




# gender gap in mobilization and persuasion -> gender gap
{
  DiagrammeR("
             graph LR;
             GMob(Gender Gap in Mobilization) --> GG(Net Gender Gap);
             GPer(Gender Gap in Persuasion) --> GG;
             Wmob(Net Democratic <br> Mobilization, Women) --> GMob;
             Mmob(Net Democratic <br> Mobilization, Men) --> GMob;
             Wper(Net Democratic <br> Persuasion, Women) --> GPer;
             Mper(Net Democratic <br> Persuasion, Men) --> GPer
             ")
}








