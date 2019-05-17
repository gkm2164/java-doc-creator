package com.kakao.bengo.godoc.exceptions;

public class TokenNoMoreToConsumeException extends Exception {
    public TokenNoMoreToConsumeException(String message) {
        super(message);
    }
}