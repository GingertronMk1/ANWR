<?php

class DefaultLogin
{
    /** Print login form.
     */
    public function loginForm()
    {
        echo '<p hidden data-plugin="default-login">Plugin on</p>';
        echo "<table cellspacing='0' class='layout'>";
        echo $this->loginFormField(
            'driver',
            '',
            'sqlite'
        );
        echo $this->loginFormField(
            'username',
            '',
            'admin'
        );
        echo $this->loginFormField(
            'password',
            '',
            'admin'
        );
        echo $this->loginFormField(
            'db',
            '',
            './db/database.sqlite'
        );
        echo '</table>';
        echo "<p><input type='submit' value='Login'>";

        return 0;
    }

    public function loginFormField(string $name, string $envValue = '', string $value = ''): string
    {
        $inputValue = $value;
        if (!empty($envValue) && isset($_ENV[$envValue])) {
            $inputValue = $_ENV[$envValue];
        }

        $nameAttr = "auth[{$name}]";

        return <<<HTML
            <tr>
                <td>
                    {$name}
                </td>
                <td>
                    <input name="{$nameAttr}" value="{$inputValue}" />
                </td>
            </tr>
        HTML;
    }
}

return new DefaultLogin();
