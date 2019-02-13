package com.rest.test;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

public class RestClient {

    private WebTarget target;

    public RestClient() {
        Client client = ClientBuilder.newClient();
        target = client.target("http://localhost:8080").path("entry-point");
    }

    public String calculate(String op, Integer left, Integer right) {
        try {
            Response response = target.path("calc")
                    .path(URLEncoder.encode(op,"UTF-8"))
                    .path(URLEncoder.encode(Integer.toString(left),"UTF-8"))
                    .path(URLEncoder.encode(Integer.toString(right),"UTF-8"))
                    .request(MediaType.TEXT_PLAIN).get();
            return response.readEntity(String.class);
        } catch (UnsupportedEncodingException e) {
            return null;
        }
    }

    public static void main(String[] args) {
        RestClient client = new RestClient();
        System.out.println(client.calculate("sum", 3, 3));
        System.out.println(client.calculate("subtract", 3, 3));
        System.out.println(client.calculate("divide", 3, 3));
        System.out.println(client.calculate("multiply", 3, 3));
    }
}
