package com.rest.test;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("/entry-point")
public class EntryPoint {

    @GET
    @Path("test")
    @Produces(MediaType.TEXT_PLAIN)
    public String test() {
        return "Hi, Statnett people!";
    }

    @GET
    @Path("/calc/{op}/{left}/{right}")
    public Integer calculate(
            @PathParam("op") String op,
            @PathParam("left") Integer left,
            @PathParam("right") Integer right) {
        return doCalc(op, left, right);
    }

    private Integer doCalc(String op, Integer left, Integer right) {
        if (op.equalsIgnoreCase("subtract")) {
            return (left - right);
        } else if (op.equalsIgnoreCase("multiply")) {
            return (left * right);
        } else if (op.equalsIgnoreCase("divide")) {
            return (left / right);
        } else {
            return (left + right);
        }
    }

}