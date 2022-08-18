FROM openjdk:11-jre-slim
COPY rabbit2rho.jar /
COPY mapping-rules.txt /
COPY top.rhox /
COPY call.rho /
ENTRYPOINT ["java", "-jar", "/rabbit2rho.jar"]