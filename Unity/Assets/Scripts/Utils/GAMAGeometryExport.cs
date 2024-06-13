using System;
using System.Collections.Generic;
using UnityEngine;
using WebSocketSharp;
using Newtonsoft.Json.Linq;
using Newtonsoft.Json;
using System.Linq;

public class GAMAGeometryExport : ConnectionWithGama
{

    protected ConnectionParameter parameters = null;
    protected CoordinateConverter converter;

    // optional: define a scale between GAMA and Unity for the location given
    public float GamaCRSCoefX = 1.0f;
    public float GamaCRSCoefY = 1.0f;
    public float GamaCRSOffsetX = 0.0f;
    public float GamaCRSOffsetY = 0.0f;

    private bool continueProcess = true;
    GameObject objectToSend;



    public void ManageGeometries(GameObject objectToSend_, string ip_, string port_, float x, float y, float ox, float oy)
    {
        objectToSend = objectToSend_;
        parameters = null;

        ip = ip_;
        port = port_;
        GamaCRSCoefX = x;
        GamaCRSCoefY = y;
        GamaCRSOffsetX = ox;
        GamaCRSOffsetY = oy;
        socket = new WebSocket("ws://" + ip + ":" + port + "/");

        socket.Connect();
        continueProcess = true; 

        socket.OnMessage += HandleReceivedMessage;

       
        System.Threading.Thread.Sleep(5000);
        Dictionary<string, string> args = new Dictionary<string, string> {
                    {"id", "geomExport"}
                  };
        SendExecutableAsk("create_init_player", args);
        int i = 0;
        while (continueProcess)
        {
            i++;
            ExportGeoms();
            if (i > 50000) break;
        }  
    }


    private void ExportGeoms()
    {
        if (parameters != null && objectToSend != null)
        {
            string message = "";
            
            UnityGeometry ug = new UnityGeometry(objectToSend, converter);
         
            message = ug.ToJSON();

            Dictionary<string, string> args = new Dictionary<string, string> {
                    {"geoms", message}
                  };
          
            SendExecutableAsk("receive_geometries", args);

           
            continueProcess = false;

        }
    }

    private void HandleServerMessageReceived(String content)
    {

        string firstKey = "";
        if (content.Contains("points"))
            firstKey = "points";
        else if (content.Contains("precision"))
            firstKey = "precision";

        switch (firstKey)
        {
            // handle general informations about the simulation
            case "precision":

                parameters = ConnectionParameter.CreateFromJSON(content);
                converter = new CoordinateConverter(parameters.precision, GamaCRSCoefX, GamaCRSCoefY, GamaCRSOffsetX, GamaCRSOffsetY);
                Debug.Log("Received parameter data");
                break;

            // handle geometries sent by GAMA at the beginning of the simulation
           
        }

    }

    protected void HandleReceivedMessage(object sender, MessageEventArgs e)
    {

        if (e.IsText)
        {
            JObject jsonObj = JObject.Parse(e.Data);
            string type = (string)jsonObj["type"];


            if (type.Equals("SimulationOutput"))
            {
                JValue content = (JValue)jsonObj["content"];
                foreach (String mes in content.ToString().Split(MessageSeparator))
                {
                    if (!mes.IsNullOrEmpty())
                        HandleServerMessageReceived(mes);
                }
            }
        }
    }

}