package models.glasso;

import java.util.*;
import java.io.*;


public class ContinuousMRFNode implements Comparable<ContinuousMRFNode>, Serializable
{
    private final String variable;
    private final int index;
    //private double pdf;
    private ContinuousMRFNode[] neighbors;
    private int[] neighborsIndices;
    private double value;
    private double score;


    public ContinuousMRFNode(String var, int nodeIndex, ArrayList<ContinuousMRFNode> neighboringNodes)
    {
        variable = var;
        //pdf = 0;
        index = nodeIndex;
        neighbors = neighboringNodes.toArray(new ContinuousMRFNode[neighboringNodes.size()]);
        Arrays.sort(neighbors);
        neighborsIndices = new int[neighbors.length];
        for(int i = 0; i < neighborsIndices.length; i++)
            neighborsIndices[i] = neighbors[i].getIndex();
        value = 0;
    }


    @Override
    public int compareTo(ContinuousMRFNode node)
    {
        double otherIndex = node.getIndex();
        if(index < otherIndex)
            return -1;
        else if(index == otherIndex)
            return 0;
        else return 1;
    }

    public boolean equals(ContinuousMRFNode node)
    {
        if(node.getIndex() == index)
            return true;
        else
            return false;
    }

    public String getVariable()
    {
        return variable;
    }

    public int getIndex()
    {
        return index;
    }

    public double getValue()
    {
        return value;
    }

    public void setValue(double val)
    {
        value = val;
    }

    protected double getScore()
    {
        return score;
    }

    protected void setScore(double s)
    {
        score = s;
    }

    public ContinuousMRFNode[] getNeighbors()
    {
        return neighbors;
    }

    /*public void setNeighbors(ArrayList<ContinuousMRFNode> neighboringNodes)
    {
        neighbors = neighboringNodes;
    }*/

    protected void addNeighbor(ContinuousMRFNode node)
    {
        ContinuousMRFNode[] newNeighbors = new ContinuousMRFNode[neighbors.length+1];
        System.arraycopy(neighbors, 0, newNeighbors, 0, neighbors.length);
        newNeighbors[neighbors.length] = node;
        neighbors = newNeighbors;
        Arrays.sort(neighbors);
        neighborsIndices = new int[neighbors.length];
        for(int i = 0; i < neighbors.length; i++)
            neighborsIndices[i] = neighbors[i].getIndex();
    }

    protected void removeNeighbor(ContinuousMRFNode node)
    {
        ContinuousMRFNode[] newNeighbors = new ContinuousMRFNode[neighbors.length-1];
        for(int i = 0, j = 0; i < neighbors.length; i++, j++)
        {
            if(neighbors[i].equals(node))
                j--;
            else
                newNeighbors[j] = neighbors[i];
        }
        neighbors = newNeighbors;
        neighborsIndices = new int[neighbors.length];
        for(int i = 0; i < neighbors.length; i++)
            neighborsIndices[i] = neighbors[i].getIndex();
    }

    public double[] getCondition()
    {
        double[] condition = new double[neighbors.length];
        for(int i = 0; i < condition.length; i++)
            condition[i] = neighbors[i].getValue();
        return condition;
    }

    /*public void initializeIndices()
    {
        for(int i = 0; i < neighborsIndices.length; i++)
            neighborsIndices[i] = neighbors.get(i).getIndex();
        //Arrays.sort(neighborsIndices);
        //Arrays.sort(indices);
    }

    private void addNeighbor(ContinuousMRFNode node)
    {
        if(!neighbors.contains(node))
            neighbors.add(node);
        ArrayList<ContinuousMRFNode> otherNeighbors = node.getNeighbors();
        if(!otherNeighbors.contains(this))
            otherNeighbors.add(this);
    }

    private void removeNeighbor(ContinuousMRFNode node)
    {
        neighbors.remove(node);
        ArrayList<ContinuousMRFNode> otherNeighbors = node.getNeighbors();
        otherNeighbors.remove(this);
    }

    private boolean isNeighborOf(ContinuousMRFNode node)
    {
        if(neighbors.contains(node))
            return true;
        else return false;
    }

    public int[] getIndices()
    {
        return indices;
    }*/

    public int[] getNeighborsIndices()
    {
        return neighborsIndices;
    }

    /*public boolean hasSameNeighbors(ContinuousMRFNode otherNode)
    {
        if(neighborsIndices.length != otherNode.neighborsIndices.length)
            return false;
        for(int i = 0; i < neighborsIndices.length; i++)
            if(neighborsIndices[i] != otherNode.neighborsIndices[i])
                return false;
        return true;
    }*/
}