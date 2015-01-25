package models.glasso;


import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.StringTokenizer;

public class InverseCovarianceMatrix {

    private ContinuousMRFNode[] nodes;
    private double priorProbability;
    private HashMap<String, ContinuousMRFNode> variablesAndNodes;
    private double[][] dataset;
    //private HashMap<ContinuousMRFNode, ninofreno.gmm.GMM[]> nodesAndGMMs;


    //creates a NonparanormalMRF with a complete graph
    public InverseCovarianceMatrix(double[][] data)
    {
        int numberOfVariables = data[0].length;
        ArrayList<String> variables = new ArrayList(numberOfVariables);
        for(int i = 0; i < numberOfVariables; i++)
            variables.add("X" + i);
        nodes = new ContinuousMRFNode[numberOfVariables];
        variablesAndNodes = new HashMap(2*numberOfVariables);
        // nodesAndGMMs = new HashMap(2*numberOfVariables);
        for(int i = 0; i < numberOfVariables; i++)
        {
            String variable = variables.get(i);
            nodes[i] = new ContinuousMRFNode(variable, i, new ArrayList());
            variablesAndNodes.put(variable, nodes[i]);
        }
        this.connectAllNodes();
        dataset = data;
        //for(int i = 0; i < numberOfVariables; i++)
            //this.initializeGMMs(nodes[i]);
        priorProbability = 0;
    }


    private void connectAllNodes()
    {
        for(int i = 0; i < nodes.length; i++)
        {
            for(int j = i+1; j < nodes.length; j++)
            {
                nodes[i].addNeighbor(nodes[j]);
                nodes[j].addNeighbor(nodes[i]);
            }
        }
    }


    public double[][] R_glassoGetInverseCovarianceMatrix()
    {
        double[][] inverseCovarianceMatrix = new double[nodes.length][nodes.length];


        try
        {
            PrintWriter writer = new PrintWriter(new FileWriter("R_tmp.data"));
            writer.print(nodes[0].getVariable());
            for(int i = 1; i < nodes.length; i++)
                writer.print("\t" + nodes[i].getVariable());
            writer.println();
            for(int i = 0; i < dataset.length; i++)
            {
                writer.print(dataset[i][0]);
                for(int j = 1; j < nodes.length; j++)
                {
                    writer.print("\t" + dataset[i][j]);
                }
                writer.println();
            }
            writer.close();
        }
        catch(IOException e)
        {
            e.printStackTrace();
        }

        try
        {
            PrintWriter writer = new PrintWriter(new FileWriter("R_tmp.R"));
            writer.println("library(glasso)");
            writer.println("R_dataset = read.table(\"R_tmp.data\", header=TRUE)");
            //writer.println("R_dataset");
            writer.println("R_covarianceMatrix = var(R_dataset)");
            //writer.println("R_covarianceMatrix");
            writer.println("R_glasso = glasso(R_covarianceMatrix, rho=0.1)");
            //writer.println("R_glasso$wi");
            writer.println("write(t(R_glasso$wi), file=\"R_glasso_wi_tmp.txt\", " +
                    "ncolumns=dim(R_glasso$wi)[[2]], sep=\"\\t\")");

            writer.close();
        }
        catch(IOException e)
        {
            e.printStackTrace();
        }



        // execute "Rscript R_tmp.R"
        try
        {
            Process p = Runtime.getRuntime().exec("Rscript R_tmp.R");

            String s;

            // read the output from the command
            BufferedReader stdInput = new BufferedReader(new InputStreamReader(p.getInputStream()));
            //System.out.println("********************************************************");
            //System.out.println("Here is the standard output of the command:\n");
            while((s = stdInput.readLine()) != null)
            {
                System.out.println(s);
            }
            //System.out.println("********************************************************");
            //System.out.println();
            stdInput.close();

            // read any errors from the command
            BufferedReader stdError = new BufferedReader(new InputStreamReader(p.getErrorStream()));
            //System.out.println("********************************************************");
            //System.out.println("Here is the standard error of the command (if any):\n");
            while((s = stdError.readLine()) != null)
            {
                System.out.println(s);
            }
            //System.out.println("********************************************************");
            //System.out.println();
            stdError.close();
        }
        catch(IOException e)
        {
            e.printStackTrace();
        }


        try
        {
            BufferedReader inputReader = new BufferedReader(new FileReader("R_glasso_wi_tmp.txt"));
            for(int i = 0; i < inverseCovarianceMatrix.length; i++)
            {
                String line = inputReader.readLine();
                StringTokenizer t = new StringTokenizer(line, "\t");
                for(int j = 0; j < inverseCovarianceMatrix[i].length; j++)
                    inverseCovarianceMatrix[i][j] = Double.parseDouble(t.nextToken());
            }
            inputReader.close();
        }
        catch(IOException e)
        {
            e.printStackTrace();
        }


        return inverseCovarianceMatrix;
    }


    public static void main(String[] args) {
        double [][] data = {
                {1,3,5,2,2},
                {1,1,5,2,2},
                {1,2,3,4,2},
                {2,6,5,2,2}
        };

        InverseCovarianceMatrix M = new InverseCovarianceMatrix( data);

        double[][] res = M.R_glassoGetInverseCovarianceMatrix();

        for(int i = 0; i < res.length; i++) {
            for(int j = 0; j < res[0].length; j++)
            {
                System.out.print(res[i][j] + ", ");
            }
            System.out.println();
        }
    }




}
